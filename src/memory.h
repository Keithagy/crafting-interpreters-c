#ifndef clox_memory_h
#define clox_memory_h

#include "common.h"
#include "object.h"
#define FREE(type, pointer) reallocate(pointer, sizeof(type), 0)
#define GROW_CAPACITY(capacity) ((capacity) < 8 ? 8 : (capacity) * 2)
#define ALLOCATE(type, count)                                                  \
  (type *)reallocate(NULL, 0, sizeof(type) * (count))
/**
 * This macro pretties up a function call to `reallocate` where the real work
 * happens. The macro itself takes care of getting the size of the array's
 * element type and casting the resulting void* back to a pointer of the right
 * */
#define GROW_ARRAY(type, pointer, oldCount, newCount)                          \
  (type *)reallocate(pointer, sizeof(type) * (oldCount),                       \
                     sizeof(type) * (newCount))
#define FREE_ARRAY(type, pointer, oldCount)                                    \
  reallocate(pointer, sizeof(type) * (oldCount), 0)

void *reallocate(void *pointer, size_t oldSize, size_t newSize);
void markObject(Obj *object);
void markValue(Value value);
void collectGarbage();
void freeObject(Obj *object);
void freeObjects();
#endif // !clox_memory_h
