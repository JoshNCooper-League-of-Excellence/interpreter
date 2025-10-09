#ifndef TYPE_H
#define TYPE_H
#include "list.h"
#include <stddef.h>
#include <stdlib.h>
struct Binding;

typedef enum {
  TYPE_EXTENSION_POINTER,
  TYPE_EXTENSION_ARRAY,
} Type_Extension;

typedef struct {
  enum {
    TYPE_INT,
    TYPE_STRING,
    TYPE_VOID,
    TYPE_FUNCTION,
    TYPE_STRUCT,
  } tag;
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
  bool is_varargs: 1;
} Function_Type;

typedef struct {
  const char *name;
  Type *type;
} Struct_Member;

DEFINE_LIST(Struct_Member);

typedef struct {
  Type base;
  Struct_Member_list members;
} Struct_Type;

#endif