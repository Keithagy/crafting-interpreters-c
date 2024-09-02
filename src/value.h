#ifndef clox_value_h
#define clox_value_h

#include "common.h"
// The name "Obj" itself refers to a struct that contains the state shared
// across all object types. It's sort of like the "base class for objects".
// Because of some cyclic dependencies between values and objects, we
// forward-declare it in the "value" module.
typedef struct Obj Obj;
typedef struct ObjString ObjString;
typedef enum {
  VAL_BOOL,
  VAL_NIL,
  VAL_NUMBER,
  VAL_OBJ,
} ValueType;
typedef struct {
  ValueType type;
  union {
    bool boolean;
    double number;
    Obj *obj;
  } as; // value as boolean, value as number, and so on
} Value;

// These check correctness of downcast to prevent unsafe memory access
#define IS_BOOL(value) ((value).type == VAL_BOOL)
#define IS_NIL(value) ((value).type == VAL_NIL)
#define IS_NUMBER(value) ((value).type == VAL_NUMBER)
#define IS_OBJ(value) ((value).type == VAL_OBJ)

// These downcast Values into specific types
#define AS_BOOL(value) ((value).as.boolean)
#define AS_NUMBER(value) ((value).as.number)
#define AS_OBJ(value) ((value).as.obj)

// These upcast specific union members into the less specific Value type
#define BOOL_VAL(value) ((Value){VAL_BOOL, {.boolean = (value)}})
#define NIL_VAL ((Value){VAL_NIL, {.number = 0}})
#define NUMBER_VAL(value) ((Value){VAL_NUMBER, {.number = (value)}})
#define OBJ_VAL(object)                                                        \
  ((Value){VAL_OBJ, {.obj = (Obj *)object}}) // note here that we need `object`
                                             // to be a bare Obj pointer.

typedef struct {
  int capacity;
  int count;
  Value *values;
} ValueArray;
void initValueArray(ValueArray *array);
void writeValueArray(ValueArray *array, Value value);
void freeValueArray(ValueArray *array);
void printValue(Value value);
bool valuesEqual(Value a, Value b);

#endif
