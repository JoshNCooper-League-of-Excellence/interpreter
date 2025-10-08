#ifndef TYPE_H
#define TYPE_H
#include "list.h"
#include <stddef.h>
#include <stdlib.h>

struct Binding;

typedef struct {
  size_t index;
  const char *name;
  struct Binding *binding;
} Type;

typedef Type* Type_Ptr;

DEFINE_LIST(Type_Ptr)


typedef struct {
  Type base;
  Type_Ptr_list parameters;
  Type *returns;
} Function_Type;





#endif