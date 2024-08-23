#ifndef clox_line_h
#define clox_line_h

#include "common.h"

typedef struct {
  int line;
  int instructionCount; // adding instruction count instead of marking the
                        // starting instruction offset means you can't
                        // binary-search this. ah well
} LineInstructionCount;

// Observe that line counts should strictly increase.
// That means you can get pretty far along appending strictly to the tail
typedef struct {
  int count;
  int capacity;
  LineInstructionCount *lineInstructionCounts;
} LineInstructionCountArray;

void initLineInstructionCountArray(LineInstructionCountArray *lica);
void freeLineInstructionCountArray(LineInstructionCountArray *lica);
void addInstructionForLine(LineInstructionCountArray *lica, int line);
int getLineByOffset(LineInstructionCountArray *lica, int offset);

#endif
