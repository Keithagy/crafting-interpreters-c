#ifndef clox_chunk_h
#define clox_chunk_h

#include "common.h"
#include "value.h"

/**
 * In our bytecode format, each instruction has a one-byte operation code
 * (universally shortened to `opcode`). That number controls what kind of
 * instruction we're dealing with: add, subtract, lookup variable, etc.
 * */
typedef enum {
  OP_CONSTANT, // Needs an *operand* to know which constant to load, i.e.
               // parameterizing what the instruction does.
  OP_NIL,
  OP_TRUE,
  OP_FALSE,
  OP_GET_UPVALUE,
  OP_SET_UPVALUE,
  OP_EQUAL,
  OP_GREATER,
  OP_LESS,
  OP_NEGATE,
  OP_ADD,
  OP_SUBTRACT,
  OP_MULTIPLY,
  OP_DIVIDE,
  OP_NOT,
  OP_DEFINE_GLOBAL,
  OP_GET_GLOBAL,
  OP_SET_GLOBAL,
  OP_GET_LOCAL,
  OP_SET_LOCAL,
  OP_JUMP_IF_FALSE,
  OP_JUMP,
  OP_LOOP,
  OP_POP,
  OP_CALL,
  OP_PRINT,
  OP_CLOSURE,
  OP_RETURN,
} OpCode;

typedef struct {
  int offset;
  int line;
} LineStart;

/**
 * Bytecode is a series of instructions. This struct provides a wrapper around
 * an array of bytes. Since we don't know how big the array needs to be before
 * we start compiling a chunk, it must be dynamic. Dynamic Arrays provide:
 * - Cache-friendly, dense storage
 * - Constant-time indexed element lookup
 * - Constant-time (amortized) appending to the end of the array
 * */
typedef struct {
  int count;
  int capacity;
  uint8_t *code;
  ValueArray constants;
  int lineCount;
  int lineCapacity;
  LineStart *lines;
} Chunk;
int getLineByOffset(Chunk *chunk, int instruction);
void initChunk(Chunk *chunk);
void freeChunk(Chunk *chunk);
void writeChunk(Chunk *chunk, u_int8_t byte, int line);
int addConstant(Chunk *chunk, Value value);

#endif
