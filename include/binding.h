#ifndef BINDING_H
#define BINDING_H

#include "lexer.h"
#include "list.h"
#include "type.h"
#include <stddef.h>

struct Ast;

typedef struct {
  size_t index;
  Type *type;
  struct Ast *ast;
} Binding;

DEFINE_LIST(Binding)

struct Ast;
typedef struct Ast *Ast_Ptr;
DEFINE_LIST(Ast_Ptr);

typedef struct {
  Type *string_type;
  Type *integer_type;
  Type_Ptr_list type_table;
  Binding_list bindings;
  Ast_Ptr_list asts;
} Context;

Ast_Ptr ast_alloc(Context *context, int tag, Span span);

inline static Type *type_alloc(Context *context) {
  Type *type = malloc(sizeof(Type));
  type->index = context->type_table.length;
  LIST_PUSH(context->type_table, type);
  return type;
}
inline static Function_Type *function_type_alloc(Context *context) {
  Function_Type *type = malloc(sizeof(Function_Type));
  type->base.index = context->type_table.length;
  LIST_PUSH(context->type_table, (Type *)type);
  return type;
}

#endif