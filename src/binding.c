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

Binding_Ptr bind_variable(Context *context, Binding binding) {
  Binding_Ptr ptr = malloc(sizeof(Binding));
  memset(ptr, 0, sizeof(Binding));
  memcpy(ptr, &binding, sizeof(Binding));

  ptr->index = context->variables;
  context->variables++;

  if (binding.thir) {
    binding.thir->binding = ptr;
  }
  if (binding.ast) {
    binding.ast->binding = ptr;
  }

  LIST_PUSH(context->bindings, ptr);
  return ptr;
}
Binding_Ptr bind_function(Context *context, Binding binding, bool is_extern) {
  Binding_Ptr ptr = malloc(sizeof(Binding));
  memset(ptr, 0, sizeof(Binding));
  memcpy(ptr, &binding, sizeof(Binding));

  if (!is_extern) { // extern functions have no associations
    ptr->index = context->functions;
    context->functions++;
  }

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
  type->base.tag = TYPE_FUNCTION;
  LIST_PUSH(context->type_table, (Type *)type);
  return type;
}

Struct_Type *struct_type_alloc(Context *context, const char *name) {
  Struct_Type *type = malloc(sizeof(Struct_Type));
  memset(type, 0, sizeof(Struct_Type));
  type->base.index = context->type_table.length;
  type->base.tag = TYPE_STRUCT;
  type->base.name = name;
  LIST_PUSH(context->type_table, (Type *)type);
  return type;
}

void context_initialize(Context *context) {

  Type *string_type = type_alloc(context);
  context->string_type = string_type;

  Type *integer_type = type_alloc(context);
  context->integer_type = integer_type;

  integer_type->name = "int";
  integer_type->tag = TYPE_INT;

  Type *u8_type = type_alloc(context);
  context->byte_type = u8_type;
  u8_type->name = "byte";
  u8_type->tag = TYPE_BYTE;


  Type *u8_ptr_type = type_alloc(context);
  context->string_type = u8_ptr_type;
  u8_ptr_type->name = "byte";
  LIST_PUSH(u8_ptr_type->extensions, TYPE_EXT_POINTER);
  u8_ptr_type->tag = TYPE_BYTE;

  Type *void_type = type_alloc(context);
  context->void_type = void_type;
  void_type->name = "void";
  void_type->tag = TYPE_VOID;

  Type *bool_type = type_alloc(context);
  context->bool_type = bool_type;
  bool_type->name = "bool";
  bool_type->tag = TYPE_BOOL;
}