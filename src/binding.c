#include "binding.h"
#include "ast.h"
#include "thir.h"
#include "type.h"

Ast *ast_alloc(Context *context, int tag, Span span) {
  struct Ast *ast = malloc(sizeof(struct Ast));
  memset(ast, 0, sizeof(Ast));
  ast->index = context->ast_list.length;
  ast->span = span;
  ast->tag = tag;
  LIST_PUSH(context->ast_list, ast);
  return ast;
}

Thir *thir_alloc(Context *context, int tag, Span span) {
  Thir *thir = malloc(sizeof(Thir));
  memset(thir, 0, sizeof(Ast));
  thir->index = context->thir_list.length;
  thir->span = span;
  thir->tag = tag;
  LIST_PUSH(context->thir_list, thir);
  return thir;
}

Binding_Ptr binding_alloc(Context *context, Binding binding) {
  Binding_Ptr ptr = malloc(sizeof(Binding));
  memset(ptr, 0, sizeof(Binding));
  *ptr = binding;
  ptr->index = context->bindings.length;

  if (binding.thir) {
    binding.thir->binding = ptr;
  }
  if (binding.ast) {
    binding.ast->binding = ptr;
  }

  LIST_PUSH(context->bindings, ptr);
  return ptr;
}

Type *type_alloc(Context *context) {
  Type *type = malloc(sizeof(Type));
  memset(type, 0, sizeof(Type));
  type->index = context->type_table.length;
  LIST_PUSH(context->type_table, type);
  return type;
}

Function_Type *function_type_alloc(Context *context) {
  Function_Type *type = malloc(sizeof(Function_Type));
  memset(type, 0, sizeof(Function_Type));
  type->base.index = context->type_table.length;
  LIST_PUSH(context->type_table, (Type *)type);
  return type;
}