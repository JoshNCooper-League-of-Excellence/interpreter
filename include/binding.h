#ifndef BINDING_H
#define BINDING_H

#include "lexer.h"
#include "list.h"
#include "type.h"
#include <stddef.h>
#include <string.h>

struct Thir;
typedef struct Thir *Thir_Ptr;
DEFINE_LIST(Thir_Ptr);

struct Ast;
typedef struct Ast *Ast_Ptr;
DEFINE_LIST(Ast_Ptr);

typedef struct Binding {
  const char *name;
  size_t index;
  Type *type;
  Ast_Ptr ast;
  Thir_Ptr thir;
} Binding;

typedef Binding *Binding_Ptr;

DEFINE_LIST(Binding_Ptr)

typedef struct {
  Type *string_type;
  Type *integer_type;
  Type_Ptr_list type_table;
  Binding_Ptr_list bindings;

  size_t variables, functions;

  Ast_Ptr_list ast_list;
  Thir_Ptr_list thir_list;
} Context;


static inline bool try_find_type(Context *context, const char *name, Type **out) {
  LIST_FOREACH(context->type_table, type) {
    if (strcmp(name, type->name) == 0) {
      *out = type;
      return true;
    }
  }
  return false;
}
Ast_Ptr ast_alloc(Context *context, int tag, Span span);
Thir_Ptr thir_alloc(Context *context, int tag, Span span);
Type *type_alloc(Context *context);
Function_Type *function_type_alloc(Context *context);

Binding_Ptr bind_variable(Context *context, Binding binding);
Binding_Ptr bind_function(Context *context, Binding binding);


#endif