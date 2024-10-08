#include "./memory.h"
#include "compiler.h"
#include "object.h"
#include "value.h"
#include "vm.h"
#include <stdlib.h>

#ifdef DEBUG_LOG_GC
#include "debug.h"
#include <stdio.h>
#endif

void *reallocate(void *pointer, size_t oldSize, size_t newSize) {
  if (newSize > oldSize) {
#ifdef DEBUG_STRESS_GC
    // Collecting right before allocation is the classic way to wire a GC into a
    // VM. You're already calling into the memory manager, so it's an easy place
    // to hook in the code. Also, allocation is the only time when you really
    // need some freed up memory so that you can resuse it. If you don't use
    // allocation to trigger a GC, you have to make sure every possible place in
    // code where you can loop and allocate memory also has a way to trigger the
    // collector. Otherwise, the VM can get into a starved state where it needs
    // more memory but never collects any.
    collectGarbage();
#endif
  }
  if (newSize == 0) {
    free(pointer);
    return NULL;
  }

  void *result = realloc(pointer, newSize);
  if (result == NULL) {
    exit(1);
  }
  return result;
}
void markObject(Obj *object) {
  if (object == NULL) {
    return;
  }
#ifdef DEBUG_LOG_GC
  printf("%p mark ", (void *)object);
  printValue(OBJ_VAL(object));
  printf("\n");
#endif
  object->isMarked = true;
}
void markValue(Value value) {
  if (IS_OBJ(value)) {
    markObject(AS_OBJ(value));
  }
}

void freeObject(Obj *object) {
#ifdef DEBUG_LOG_GC
  printf("%p free type %d\n", (void *)object, object->type);
#endif /* ifdef DEBUG_LOG_GC */
  switch (object->type) {
  case OBJ_STRING: {
    ObjString *string = (ObjString *)object;
    reallocate(object, sizeof(ObjString) + string->length + 1, 0);
    break;
  }
  case OBJ_FUNCTION: {
    ObjFunction *function = (ObjFunction *)object;
    freeChunk(&function->chunk);
    FREE(ObjFunction, object);
    break;
  }
  case OBJ_NATIVE: {
    FREE(ObjNative, object);
    break;
  }
  case OBJ_CLOSURE: {
    // We free only the ObjClosure itself, not the ObjFunction. That's because
    // the closure doesn't own the function. There may be multiple closures that
    // all reference the same function, and none of them claims any special
    // privilege over it. We can't free the ObjFunction until all objects
    // referencing it are gone -- including even the surrounding function whose
    // constant table contains it. Tracking that sounds tricky, and it is!
    // That's why we'll write a garbage collector soon to manage it for us.
    ObjClosure *closure = (ObjClosure *)object;
    FREE_ARRAY(ObjUpvalue *, closure->upvalues, closure->upvalueCount);
    FREE(ObjClosure, object);
    break;
  }
  case OBJ_UPVALUE:
    FREE(ObjUpvalue, object);
    break;
  }
}
static void markRoots() {
  for (Value *slot = vm.stack; slot < vm.stackTop; slot++) {
    markValue(*slot);
  }
  for (int i = 0; i < vm.frameCount; i++) {
    markObject((Obj *)vm.frames[i].closure);
  }
  for (ObjUpvalue *upvalue = vm.openUpvalues; upvalue != NULL;
       upvalue = upvalue->next) {
    markObject((Obj *)upvalue);
  }
  markTable(&vm.globals);
  markCompilerRoots();
}
void collectGarbage() {
#ifdef DEBUG_LOG_GC
  printf("-- gc begin\n");
#endif

  markRoots();

#ifdef DEBUG_LOG_GC
  printf("-- gc end\n");
#endif
}
void freeObjects() {
  Obj *object = vm.objects;
  while (object != NULL) {
    Obj *next = object->next;
    freeObject(object);
    object = next;
  }
}
