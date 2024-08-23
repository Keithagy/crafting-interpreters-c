#include "chunk.h"
#include "./memory.h"
#include "line.h"
#include "value.h"
#include <stdlib.h>

void initChunk(Chunk *chunk) {
  chunk->count = 0;
  chunk->capacity = 0;
  chunk->code = NULL;
  initValueArray(&chunk->constants);
  initLineInstructionCountArray(&chunk->lines);
}
void freeChunk(Chunk *chunk) {
  FREE_ARRAY(uint8_t, chunk->code, chunk->capacity);
  freeLineInstructionCountArray(&chunk->lines);
  freeValueArray(&chunk->constants);
  initChunk(chunk);
}
void writeChunk(Chunk *chunk, u_int8_t byte, int line) {
  if (!(chunk->capacity > chunk->count)) {
    int oldCapacity = chunk->capacity;
    chunk->capacity = GROW_CAPACITY(oldCapacity);
    chunk->code =
        GROW_ARRAY(uint8_t, chunk->code, oldCapacity, chunk->capacity);
  }
  chunk->code[chunk->count] = byte;
  addInstructionForLine(&chunk->lines, line);
  chunk->count++;
}

int addConstant(Chunk *chunk, Value value) {
  writeValueArray(&chunk->constants, value);

  // After we add the constant, we return the index where the constant
  // was appended so that we can locate that same constant later.
  return chunk->constants.count - 1;
}
