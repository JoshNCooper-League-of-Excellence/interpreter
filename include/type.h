#ifndef TYPE_H
#define TYPE_H
#include "list.h"
#include "string_builder.h"
#include <stddef.h>
#include <stdlib.h>
struct Binding;

typedef enum {
  TYPE_EXT_POINTER,
  TYPE_EXT_ARRAY,
} Type_Extension;

DEFINE_LIST(Type_Extension);

typedef struct Type {
  enum {
    TYPE_INT,
    TYPE_BYTE,
    TYPE_VOID,
    TYPE_BOOL,
    TYPE_FUNCTION,
    TYPE_STRUCT,
  } tag;
  size_t index;
  const char *name;
  struct Type *pointee;
  struct Binding *binding;
  Type_Extension_list extensions;
} Type;

inline static bool type_is_pointer(Type *type) {
  return type->extensions.length &&
         type->extensions.data[type->extensions.length - 1] == TYPE_EXT_POINTER;
}

inline static bool type_is_pointer_of_depth(Type *type, unsigned depth) {
  if (type->extensions.length < depth) {
    return false;
  }

  for (unsigned i = type->extensions.length; i > depth; --i) {
    if (type->extensions.data[i] != TYPE_EXT_POINTER) {
      return false;
    }
  }

  return true;
}

inline static void print_type(Type *type, String_Builder *sb) {
  sb_append(sb, type->name ? type->name : "<unnamed>");
  for (size_t i = 0; i < type->extensions.length; ++i) {
    if (type->extensions.data[i] == TYPE_EXT_ARRAY) {
      sb_append(sb, "[]");
    } else if (type->extensions.data[i] == TYPE_EXT_POINTER) {
      sb_append(sb, "*");
    }
  }
}

inline static bool type_is_array(Type *type) {
  return type->extensions.length &&
         type->extensions.data[type->extensions.length - 1] == TYPE_EXT_POINTER;
}

inline static bool type_has_extensions(Type *type) {
  return type->extensions.length;
}

typedef Type *Type_Ptr;

DEFINE_LIST(Type_Ptr)

typedef struct {
  Type base;
  Type_Ptr_list parameters;
  Type *returns;
  bool is_varargs : 1;
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