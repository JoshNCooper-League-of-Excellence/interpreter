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

  Type *typer_expected_type;
} Context;

static inline bool try_find_type(Context *context, const char *name,
                                 Type **out) {
  LIST_FOREACH(context->type_table, type) {
    if (strcmp(name, type->name) == 0) {
      *out = type;
      return true;
    }
  }
  return false;
}

static inline bool try_find_function_type(Context *context,
                                          Type_Ptr_list parameter_types,
                                          Type *return_type, Function_Type **out) {
  LIST_FOREACH(context->type_table, type) {
    if (type->tag != TYPE_FUNCTION) {
      continue;
    }
    Function_Type *function = (Function_Type *)type;

    if (return_type != function->returns) {
      continue;
    }

    if (!LIST_EQ(function->parameters, parameter_types)) {
      continue;
    }

    *out = function;
    return true;
  }
  return false;
}

Ast_Ptr ast_alloc(Context *context, int tag, Span span);
Thir_Ptr thir_alloc(Context *context, int tag, Span span);
Type *type_alloc(Context *context);

Function_Type *function_type_alloc(Context *context);

Binding_Ptr bind_variable(Context *context, Binding binding);
Binding_Ptr bind_function(Context *context, Binding binding, bool is_extern);

#endif