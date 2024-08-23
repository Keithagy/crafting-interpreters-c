#include "line.h"
#include "./memory.h"
#include <stdio.h>
#include <stdlib.h>

static void addLine(LineInstructionCountArray *lica, int line) {
  if (!(lica->capacity > lica->count)) {
    int oldCapacity = lica->capacity;
    lica->capacity = GROW_CAPACITY(oldCapacity);
    lica->lineInstructionCounts =
        GROW_ARRAY(LineInstructionCount, lica->lineInstructionCounts,
                   oldCapacity, lica->capacity);
  }
  LineInstructionCount *new = malloc(sizeof(LineInstructionCount));
  new->line = line;
  new->instructionCount = 0;
  lica->lineInstructionCounts[lica->count] = *new;
  lica->count++;
}

int getLineByOffset(LineInstructionCountArray *lica, int offset) {
  int i = 0;
  while (i < lica->count) {
    offset -= lica->lineInstructionCounts[i].instructionCount;
    if (offset <= 0) {
      return lica->lineInstructionCounts[i].line;
    }
    i++;
  }
  return -1;
}

void initLineInstructionCountArray(LineInstructionCountArray *lica) {
  lica->count = 0;
  lica->capacity = 0;
  lica->lineInstructionCounts = NULL;
}

void freeLineInstructionCountArray(LineInstructionCountArray *lica) {
  FREE_ARRAY(LineInstructionCount, lica->lineInstructionCounts, lica->capacity);
  initLineInstructionCountArray(lica);
}

void addInstructionForLine(LineInstructionCountArray *lica, int line) {
  if (lica->capacity == 0 ||
      lica->lineInstructionCounts[lica->count].line != line) {
    addLine(lica, line);
  }
  lica->lineInstructionCounts[lica->count].instructionCount++;
}
