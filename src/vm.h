#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "object.h"
#include "table.h"
#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

typedef struct {
  ObjFunction *function;
  uint8_t *ip;
  Value *slots; // `slots` points into the VM's value stack at the first slot
                // that this function invocation can use. Represents scoping
                // local to a given invocation.
} CallFrame;

typedef struct {
  CallFrame frames[FRAMES_MAX];
  int frameCount;
  Value stack[STACK_MAX];
  Value *stackTop; // C does allow for array pointer to point just past end of
                   // array
  Obj *objects;
  Table globals;
  Table strings;
} VM;

typedef enum {
  INTERPRET_OK,
  INTERPRET_COMPILE_ERROR,
  INTERPRET_RUNTIME_ERROR,
} InterpretResult;
extern VM vm;
void initVM();
void freeVM();
InterpretResult interpret(const char *source);
void push(Value value);
Value pop();

#endif
