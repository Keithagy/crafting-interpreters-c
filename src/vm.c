#include "vm.h"
#include "chunk.h"
#include "common.h"
#include "debug.h"
#include <stdio.h>

// The choice to have a static VM instance is a concession for the book,
// but not necessarily a sound engineering choice for a real language
// implementation. If you're building a VM that's designed to be embedded in
// other host applications, it gives the host more flexibility if you explicitly
// take a VM pointer and pass it around.
//
// That way, the host app can control when and where memory for the VM is
// allocated, run multiple VMs in parallel, etc.
VM vm;

static void resetStack() { vm.stackTop = vm.stack; }
void push(Value value) {
  *vm.stackTop = value;
  vm.stackTop++;
}
Value pop() {
  vm.stackTop--;
  return *vm.stackTop;
}
void initVM() { resetStack(); }

void freeVM() {}

static InterpretResult run() {
// Macro to read the byte currently pointed at by `ip` and then advances the
// instruction pointer.
#define READ_BYTE() (*vm.ip++)
#define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])
  for (;;) {
#ifdef DEBUG_TRACE_EXECUTION
    printf("          ");
    for (Value *slot = vm.stack; slot < vm.stackTop; slot++) {
      printf("[ ");
      printValue(*slot);
      printf(" ]");
    }
    printf("\n");
    disassembleInstruction(vm.chunk, (int)(vm.ip - vm.chunk->code));
#endif
    uint8_t instruction;
    // Given a numeric opcode, we need to get to the right C code that
    // implements the instruction's semnatics. This process is called `decoding`
    // or `dispatching` the instruction.
    switch (instruction = READ_BYTE()) {
    case OP_CONSTANT: {
      Value constant = READ_CONSTANT();
      push(constant);
      break;
    }
    case OP_RETURN:
      printValue(pop());
      printf("\n");
      return INTERPRET_OK;
    }
  }
#undef READ_BYTE
#undef READ_CONSTANT
}

InterpretResult interpret(Chunk *chunk) {
  vm.chunk = chunk;
  vm.ip = vm.chunk->code;
  return run();
}
