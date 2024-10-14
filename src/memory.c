#include "./memory.h"
#include "compiler.h"
#include "object.h"
#include "table.h"
#include "value.h"
#include "vm.h"
#include <stdlib.h>

#ifdef DEBUG_LOG_GC
#include "debug.h"
#include <stdio.h>
#endif

#define GC_HEAP_GROW_FACTOR 2

void *reallocate(void *pointer, size_t oldSize, size_t newSize) {
  vm.bytesAllocated += newSize - oldSize;
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
  if (vm.bytesAllocated > vm.nextGC) {
    collectGarbage();
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
  if (object == NULL || object->isMarked) {
    return;
  }
#ifdef DEBUG_LOG_GC
  printf("%p mark ", (void *)object);
  printValue(OBJ_VAL(object));
  printf("\n");
#endif
  object->isMarked = true;

  // For the worklist for gray objects in the GC, we use a stack because it's
  // simplest to implement with a dynamic array in C. It works mostly like other
  // dynamic arrays we've built in Lox, except that it calls the system
  // `realloc` directly, not our `reallocate` wrapper. This is because the
  // memory for the gray stack itself is not managed by the GC. We don't want
  // growing the gray stack during a GC pass to cause the GC to recursively
  // start a new GC.
  if (vm.grayCapacity < vm.grayCount + 1) {
    vm.grayCapacity = GROW_CAPACITY(vm.grayCapacity);
    vm.grayStack =
        (Obj **)realloc(vm.grayStack, sizeof(Obj *) * vm.grayCapacity);

    // We take full responsibility for this dynamic array, incl. allocation
    // failure. If we can't create or grow the gray stack, then we can't finish
    // the garbage collection. This is bad news for the VM, but fortunately rare
    // since the gray stack tends to be pretty small. Would be nice to do
    // something more graceful, but we just abort for simplicity.
    // NOTE: To be more robust, we can allocate a "rainy day fund" block of
    // memory when we start the VM. If the gray stack allocation fails, we free
    // the rainy day block and try again. That may give us enough wiggle room on
    // the heap to create the gray stack, finish the GC, and free up more
    // memory.
    if (vm.grayStack == NULL)
      exit(1);
  }
  vm.grayStack[vm.grayCount++] = object;
}
void markValue(Value value) {
  if (IS_OBJ(value)) {
    markObject(AS_OBJ(value));
  }
}
static void markArray(ValueArray *array) {
  for (int i = 0; i < array->count; i++) {
    markValue(array->values[i]);
  }
}
// NOTE: We don't explicitly track "blackness" of an object's state.
// A black object is any object whose `isMarked` field is set and that is no
// longer in the gray stack.
static void blackenObject(Obj *object) {
#ifdef DEBUG_LOG_GC
  printf("%p blacken ", (void *)object);
  printValue(OBJ_VAL(object));
  printf("\n");
#endif
  switch (object->type) {
  case OBJ_CLOSURE: {
    ObjClosure *closure = (ObjClosure *)object;
    markObject((Obj *)closure->function);
    for (int i = 0; i < closure->upvalueCount; i++) {
      markObject((Obj *)closure->upvalues[i]);
    }
    break;
  }
  case OBJ_UPVALUE: {
    markValue(((ObjUpvalue *)object)->closed);
    break;
  }
  case OBJ_FUNCTION: {
    ObjFunction *function = (ObjFunction *)object;
    markObject((Obj *)(function->name));
    markArray(&function->chunk.constants);
    break;
  }
  // Strings and native functions have no outgoing references, so nothing to
  // traverse.
  case OBJ_NATIVE:
  case OBJ_STRING:
    break;
  case OBJ_CLASS: {
    ObjClass *klass = (ObjClass *)object;
    markObject((Obj *)klass->name);
    break;
  }
  case OBJ_INSTANCE: {
    ObjInstance *instance = (ObjInstance *)object;
    markObject((Obj *)instance->klass);
    markTable(&instance->fields);
    break;
  }
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
  case OBJ_CLASS: {
    FREE(ObjClass, object);
    break;
  }
  case OBJ_INSTANCE: {
    ObjInstance *instance = (ObjInstance *)object;
    freeTable(&instance->fields);
    FREE(ObjInstance, object);
    break;
  }
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
static void traceReferences() {
  while (vm.grayCount > 0) {
    Obj *object = vm.grayStack[--vm.grayCount];
    blackenObject(object);
  }
}
static void sweep() {
  Obj *previous = NULL;
  Obj *object = vm.objects;
  while (object != NULL) {
    if (object->isMarked) {
      object->isMarked = false;
      previous = object;
      object = object->next;
    } else {
      Obj *unreached = object;
      object = object->next;
      if (previous != NULL) {
        previous->next = object;
      } else {
        vm.objects = object;
      }
      freeObject(unreached);
    }
  }
}
void collectGarbage() {
#ifdef DEBUG_LOG_GC
  printf("-- gc begin\n");
  size_t before = vm.bytesAllocated;
#endif

  markRoots();
  traceReferences();
  tableRemoveWhite(&vm.strings);
  sweep();

  vm.nextGC = vm.bytesAllocated * GC_HEAP_GROW_FACTOR;

#ifdef DEBUG_LOG_GC
  printf("-- gc end\n");
  printf("   collected %zu bytes (from %zu to %zu) next at %zu\n",
         before - vm.bytesAllocated, before, vm.bytesAllocated, vm.nextGC);
#endif
}
void freeObjects() {
  Obj *object = vm.objects;
  while (object != NULL) {
    Obj *next = object->next;
    freeObject(object);
    object = next;
  }
  free(vm.grayStack);
}
