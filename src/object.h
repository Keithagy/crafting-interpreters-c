#ifndef clox_object_h
#define clox_object_h

#include "chunk.h"
#include "common.h"
#include "table.h"
#include "value.h"

#define OBJ_TYPE(value) (AS_OBJ(value)->type)

#define IS_BOUND_METHOD(value) (isObjType(value, OBJ_BOUND_METHOD))
#define IS_CLASS(value) (isObjType(value, OBJ_CLASS))
#define IS_CLOSURE(value) (isObjType(value, OBJ_CLOSURE))
#define IS_FUNCTION(value) (isObjType(value, OBJ_FUNCTION))
#define IS_INSTANCE(value) (isObjType(value, OBJ_INSTANCE))
#define IS_NATIVE(value) (isObjType(value, OBJ_NATIVE))
#define IS_STRING(value) (isObjType(value, OBJ_STRING))

#define AS_BOUND_METHOD(value) ((ObjBoundMethod *)AS_OBJ(value))
#define AS_CLASS(value) ((ObjClass *)AS_OBJ(value))
#define AS_CLOSURE(value) ((ObjClosure *)AS_OBJ(value))
#define AS_FUNCTION(value) ((ObjFunction *)AS_OBJ(value))
#define AS_INSTANCE(value) ((ObjInstance *)AS_OBJ(value))
#define AS_NATIVE(value) (((ObjNative *)AS_OBJ(value))->function)
#define AS_STRING(value) ((ObjString *)AS_OBJ(value))
#define AS_CSTRING(value) (((ObjString *)AS_OBJ(value))->chars)
typedef enum {
  OBJ_BOUND_METHOD,
  OBJ_CLASS,
  OBJ_CLOSURE,
  OBJ_FUNCTION,
  OBJ_STRING,
  OBJ_NATIVE,
  OBJ_UPVALUE,
  OBJ_INSTANCE,
} ObjType;

struct Obj {
  ObjType type;
  bool isMarked;
  struct Obj *next;
};

typedef struct {
  Obj obj;
  int arity;
  int upvalueCount;
  Chunk chunk;
  ObjString *name;
} ObjFunction;

typedef Value (*NativeFn)(int argCount, Value *args);

typedef struct {
  Obj obj;
  NativeFn function;
} ObjNative;

struct ObjString {
  Obj obj;
  int length;
  uint32_t hash;
  char *chars;
};
typedef struct ObjUpvalue {
  Obj obj;
  Value *location; // We know upvalues must manage closed-over variables that no
                   // longer live on the stack, which implies some amount of
                   // dynamic allocation. The easiest way to do that in our VM
                   // is by building on the object system we already have. That
                   // way, when we implement a garbage collector, it can manage
                   // memory for upvalues too.
  Value closed;
  struct ObjUpvalue *next;
} ObjUpvalue;
typedef struct {
  Obj obj;
  ObjFunction *function;
  ObjUpvalue **upvalues;
  int upvalueCount;
} ObjClosure;
typedef struct {
  Obj obj;
  ObjString *name; // not strictly needed, but lets use show the name at runtime
                   // for things like stack traces
  Table methods;
} ObjClass;
typedef struct {
  Obj obj;
  ObjClass *klass; // method lookup works through this pointer
  Table fields;
} ObjInstance;
typedef struct {
  Obj obj;
  Value receiver;
  ObjClosure *method;
} ObjBoundMethod;

ObjBoundMethod *newBoundMethod(Value receiver, ObjClosure *method);
ObjClass *newClass(ObjString *name);
ObjClosure *newClosure(ObjFunction *function);
ObjFunction *newFunction();
ObjInstance *newInstance(ObjClass *klass);
ObjNative *newNative(NativeFn function);
ObjString *copyString(const char *chars, int length);
ObjString *takeString(char *chars, int length);
ObjUpvalue *newUpvalue(Value *slot);
void printObject(Value value);
static inline bool isObjType(Value value, ObjType type) {
  return IS_OBJ(value) && AS_OBJ(value)->type == type;
}

#endif
