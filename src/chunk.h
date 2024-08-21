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
  OP_RETURN,
} OpCode;

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
  int *lines;
  ValueArray constants;
} Chunk;
void initChunk(Chunk *chunk);
void freeChunk(Chunk *chunk);
void writeChunk(Chunk *chunk, u_int8_t byte, int line);
int addConstant(Chunk *chunk, Value value);

#endif
