#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#define STACK_MAX 256

typedef struct {
  Chunk *chunk;
  // If we were trying to squeeze every ounce of speed out of our bytecode
  // interpreter, we would store `ip` in a local variable. It gets modified so
  // often during execution that we want the C compiler to keep it in a
  // register. Still, we use an actual real C pointer pointing right into the
  // middle of the bytecode array instead of something like an integer index
  // because it's faster to dereference a pointer than look up an element in an
  // array by index.
  uint8_t *ip; // >> instruction pointer, points to the next instruction, not
  // the one currently being handled
  Value stack[STACK_MAX];
  Value *stackTop; // C does allow for array pointer to point just past end of
                   // array
  Obj *objects;
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
