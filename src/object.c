#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include "memory.h"
#include "object.h"
#include "value.h"
#include "vm.h"

#define ALLOCATE_OBJ(type, objectType)                                         \
  (type *)allocateObject(sizeof(type), objectType)

static Obj *allocateObject(size_t size, ObjType type) {
  Obj *object = (Obj *)reallocate(NULL, 0, size);
  object->type = type;
  object->next = vm.objects;
  vm.objects = object;
  return object;
}

static ObjString *makeString(int length) {
  ObjString *string =
      (ObjString *)allocateObject(sizeof(ObjString) + length + 1, OBJ_STRING);
  string->length = length;
  return string;
}

static uint32_t hashString(const char *key, int length) {
  uint32_t hash = 2166136261u;
  for (int i = 0; i < length; i++) {
    hash ^= (uint8_t)key[i];
    hash *= 16777619;
  }
  return hash;
}
ObjString *copyString(int length, const char *first, ...) {
  va_list charSeqs;
  va_start(charSeqs, first);

  ObjString *string = makeString(length);
  int currentPos = 0;

  // Copy the first string
  int firstLen = strlen(first);
  memcpy(string->chars + currentPos, first, firstLen);
  currentPos += firstLen;

  // Copy subsequent strings
  const char *str = va_arg(charSeqs, const char *);
  while (str != NULL) {
    int strLen = strlen(str);
    memcpy(string->chars + currentPos, str, strLen);
    currentPos += strLen;
    str = va_arg(charSeqs, const char *);
  }

  va_end(charSeqs);

  // Ensure null termination
  string->chars[length] = '\0';

  // Calculate hash
  uint32_t hash = hashString(string->chars, length);

  // Prior allocation isn't wasted; it's necessary in order to give a comparison
  // target for the string-to-be
  ObjString *interned =
      tableFindString(&vm.strings, string->chars, length, hash);
  if (interned != NULL)
    freeObject(&string->obj);
  return interned;

  tableSet(&vm.strings, string, NIL_VAL);
  string->hash = hash;

  return string;
}

void printObject(Value value) {
  switch (OBJ_TYPE(value)) {
  case OBJ_STRING:
    printf("%s", AS_CSTRING(value));
    break;
  }
}
