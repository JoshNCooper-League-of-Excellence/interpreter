#include "binding.h"
#include "ast.h"
#include "thir.h"
#include "type.h"

Ast *ast_alloc(Context *context, int tag, Span span) {
  struct Ast *ast = arena_alloc(&context->ast_arena, sizeof(struct Ast));
  ast->index = context->ast_list.length;
  ast->span = span;
  ast->tag = tag;
  LIST_PUSH(context->ast_list, ast);
  return ast;
}

Thir *thir_alloc(Context *context, int tag, Span span) {
  Thir *thir = arena_alloc(&context->thir_arena, (sizeof(Thir)));
  thir->index = context->thir_list.length;
  thir->span = span;
  thir->tag = tag;
  LIST_PUSH(context->thir_list, thir);
  return thir;
}

Binding_Ptr bind_variable(Context *context, Binding binding) {
  Binding_Ptr ptr = arena_alloc(&context->binding_arena, sizeof(Binding));
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
  Binding_Ptr ptr = arena_alloc(&context->binding_arena, sizeof(Binding));
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
  Type *type = arena_alloc(&context->type_arena, sizeof(Type));
  type->index = context->type_table.length;
  LIST_PUSH(context->type_table, type);
  return type;
}

Function_Type *function_type_alloc(Context *context) {
  Function_Type *type = arena_alloc(&context->function_type_arena, sizeof(Function_Type));
  type->base.index = context->type_table.length;
  type->base.tag = TYPE_FUNCTION;
  LIST_PUSH(context->type_table, (Type *)type);
  return type;
}

Struct_Type *struct_type_alloc(Context *context, const char *name) {
  Struct_Type *type = arena_alloc(&context->struct_type_arena, sizeof(Struct_Type));
  type->base.index = context->type_table.length;
  type->base.tag = TYPE_STRUCT;
  type->base.name = name;
  LIST_PUSH(context->type_table, (Type *)type);
  return type;
}

void context_initialize(Context *context) {
  arena_init(&context->function_type_arena, 1024 * 1024);
  arena_init(&context->struct_type_arena, 1024 * 1024);
  arena_init(&context->ast_arena, 1024 * 1024);
  arena_init(&context->thir_arena, 1024 * 1024);
  arena_init(&context->type_arena, 1024 * 1024);
  arena_init(&context->binding_arena, 1024 * 1024);

  Type *integer_type = type_alloc(context);
  context->integer_type = integer_type;
  integer_type->name = "int";
  integer_type->tag = TYPE_INT;

  Type *u8_type = type_alloc(context);
  context->byte_type = u8_type;
  u8_type->name = "byte";
  u8_type->tag = TYPE_BYTE;

  Type *byte_ptr_type = type_alloc(context);
  context->string_type = byte_ptr_type;
  byte_ptr_type->name = "byte";
  byte_ptr_type->pointee = u8_type;
  LIST_PUSH(byte_ptr_type->extensions, TYPE_EXT_POINTER);
  byte_ptr_type->tag = TYPE_BYTE;

  Type *void_type = type_alloc(context);
  context->void_type = void_type;
  void_type->name = "void";
  void_type->tag = TYPE_VOID;

  Type *bool_type = type_alloc(context);
  context->bool_type = bool_type;
  bool_type->name = "bool";
  bool_type->tag = TYPE_BOOL;
}

Binding *get_binding(const char *identifier, Context *context) {
  if (!identifier) {
    fprintf(stderr, "error: null identifier in get_binding()\n");
    exit(1);
  }

  LIST_FOREACH(context->ast_list, ast) {
    if (!ast->binding) {
      continue;
    }
    if (ast->tag == AST_VARIABLE) {
      if (strcmp(ast->variable.name, identifier) == 0) {
        return ast->binding;
      }
    } else if (ast->tag == AST_FUNCTION) {
      if (strcmp(ast->function.name, identifier) == 0) {
        return ast->binding;
      }
    }
  }

  LIST_FOREACH(context->thir_list, thir) {
    if (!thir->binding) {
      continue;
    }

    switch (thir->tag) {
    case THIR_VARIABLE:
      if (strcmp(thir->binding->name, identifier) == 0) {
        return thir->binding;
      }
      break;
    case THIR_FUNCTION:
      if (strcmp(thir->function.name, identifier) == 0) {
        return thir->binding;
      }
      break;
    case THIR_EXTERN:
      if (strcmp(thir->extern_function.name, identifier) == 0) {
        return thir->binding;
      }
      break;
      break;
    default:
      break;
    }
  }

  LIST_FOREACH(context->type_table, type) {
    if (!type->binding) {
      continue;
    }
    if (strcmp(type->name, identifier) == 0) {
      return type->binding;
    }
  }

  return nullptr;
}