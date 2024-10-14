#ifndef clox_vm_h
#define clox_vm_h

#include "chunk.h"
#include "object.h"
#include "table.h"
#define FRAMES_MAX 64
#define STACK_MAX (FRAMES_MAX * UINT8_COUNT)

typedef struct {
  ObjClosure *closure;
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

  /*
   * On garbage collector frequency and the tradeoff between maximimizing
   * throughput (% time spent running the program and not the gc) and minimizing
   * latency (max duration of a GC pause):
   *
   * The idea is that the collector frequency automatically adjusts based on the
   * live size of the heap. We track the total number of bytes of managed memory
   * that the VM has allocated. When it goes above some threshold, we trigger a
   * GC. After that, we note how many bytes of memory remain -- how many were
   * not freed. Then we adjust the threshold to some value larger than that.
   *
   * The result is that as the amount of live memory increases, we collect less
   * frequently in order to avoid sacrificing throughput by re-traversing the
   * growing pile of live objects. As the amount of live memory goes down, we
   * collect more frequently so that we don't lose too much latency by waiting
   * too long.
   * */
  size_t bytesAllocated;
  size_t nextGC;

  Obj *objects;
  Table globals;
  Table strings;
  ObjString *initString;
  ObjUpvalue *openUpvalues;

  int grayCount;
  int grayCapacity;
  Obj **grayStack;
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
