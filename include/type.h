#ifndef TYPE_H
#define TYPE_H
#include "list.h"
#include <stddef.h>
#include <stdlib.h>

typedef struct {
  size_t index;
  const char *name;
} Type;

typedef Type* Type_Ptr;

DEFINE_LIST(Type_Ptr)

typedef struct {
  Type base;
  Type_Ptr_list parameters;
  Type *returns;
} Function_Type;

extern Type *string_type;
extern Type *integer_type;
extern Type_Ptr_list type_table;

inline static Type *type_alloc() {
  return malloc(sizeof(Type));
}

inline static Function_Type *function_type_alloc() {
  return malloc(sizeof(Function_Type));
}



#endif