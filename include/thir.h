#ifndef THIR_H
#define THIR_H

#include "binding.h"
#include "lexer.h"

typedef enum {
  THIR_PROGRAM,
  THIR_VARIABLE,
  THIR_FUNCTION,
  THIR_BLOCK,
  THIR_LITERAL,
  THIR_UNARY,
  THIR_BINARY,
  THIR_RETURN,
  THIR_CALL,
  THIR_EXTERN,
} Thir_Tag;

#include "ffi.h"

DEFINE_LIST(ffi_type);

typedef struct {
  const char *name;
  ffi_type_list parameters;
  ffi_type return_type;
  size_t index;
  void *ptr;
  Type *original_return_type;
} Extern_Function;

struct Thir;
DEFINE_LIST(Extern_Function);
extern Extern_Function_list CACHED_EXTERNS;
Extern_Function get_ffi_function_from_thir(struct Thir *thir);

typedef struct Thir {
  Binding *binding;
  Thir_Tag tag;
  Span span;
  size_t index;
  Type *type;
  union {
    Thir_Ptr_list program;
    Thir_Ptr_list block;

    struct {
      Type *return_type;
      Binding_Ptr_list parameters;
      const char *name;
      size_t index; // index of this function in CACHED_EXTERNS
    } extern_function;

    struct {
      Type *return_type;
      Binding_Ptr_list parameters;
      struct Thir *block;
      const char *name;
    } function;

    struct {
      Binding *callee;
      Thir_Ptr_list arguments;
      Type *type;
    } call;

    struct {
      const char *value;
    } literal;

    struct {
      struct Thir *left, *right;
      Token_Type op;
    } binary;

    struct {
      struct Thir *operand;
      Token_Type op;
    } unary;

    struct Thir *variable_initializer;

    struct Thir *return_value;
  };
} Thir;

Thir *type_program(struct Ast *, Context *context);
Thir *type_literal(struct Ast *, Context *context);
Thir *type_call(struct Ast *, Context *context);
Thir *type_expression(struct Ast *, Context *context);
Thir *type_identifier(struct Ast *, Context *context);
Thir *type_block(struct Ast *, Context *context);
Thir *type_function(struct Ast *, Context *context);
Thir *type_extern(struct Ast *, Context *context);
Thir *type_unary(struct Ast *, Context *context);
Thir *type_binary(struct Ast *, Context *context);
Thir *type_return(struct Ast *, Context *context);
Thir *type_variable(struct Ast *, Context *context);

#include "string_builder.h"

static const char *thir_tag_names[] = {
    "PROGRAM", "VARIABLE", "FUNCTION", "BLOCK", "LITERAL",
    "UNARY",   "BINARY",   "RETURN",   "CALL",  "EXTERN"};

static inline void print_indent_ir(String_Builder *sb, int indent) {
  for (int i = 0; i < indent; ++i) {
    sb_append(sb, "  ");
  }
}

static inline void print_ir_rec(Thir *node, String_Builder *sb, int indent) {
  if (!node) {
    print_indent_ir(sb, indent);
    sb_append(sb, "null\n");
    return;
  }
  print_indent_ir(sb, indent);
  sb_append(sb, thir_tag_names[node->tag]);
  sb_append(sb, "\n");

  switch (node->tag) {
  case THIR_EXTERN:
    print_indent_ir(sb, indent + 1);
    sb_append(sb, "name: ");
    sb_append(sb, node->extern_function.name);
    sb_append(sb, "\n");
    print_indent_ir(sb, indent + 1);
    sb_append(sb, "return_type: ");
    sb_append(sb, node->extern_function.return_type
                      ? node->extern_function.return_type->name
                      : "void");
    sb_append(sb, "\n");
    print_indent_ir(sb, indent + 1);
    sb_append(sb, "parameters:\n");
    for (size_t i = 0; i < node->extern_function.parameters.length; ++i) {
      Binding *param = node->extern_function.parameters.data[i];
      print_indent_ir(sb, indent + 2);
      sb_append(sb, param->type ? param->type->name : "unknown");
      sb_append(sb, " ");
      sb_append(sb, param->name);
      sb_append(sb, "\n");
    }
    break;
  case THIR_PROGRAM:
    for (size_t i = 0; i < node->program.length; ++i) {
      print_ir_rec(node->program.data[i], sb, indent + 1);
    }
    break;
  case THIR_BLOCK:
    for (size_t i = 0; i < node->block.length; ++i) {
      print_ir_rec(node->block.data[i], sb, indent + 1);
    }
    break;
  case THIR_LITERAL:
    print_indent_ir(sb, indent + 1);
    sb_append(sb, "value: ");
    sb_append(sb, node->literal.value);
    sb_append(sb, "\n");
    break;
  case THIR_FUNCTION:
    print_indent_ir(sb, indent + 1);
    sb_append(sb, "name: ");
    sb_append(sb, node->function.name);
    sb_append(sb, "\n");
    print_indent_ir(sb, indent + 1);
    sb_append(sb, "return_type: ");
    sb_append(sb, node->function.return_type ? node->function.return_type->name
                                             : "void");
    sb_append(sb, "\n");
    print_indent_ir(sb, indent + 1);
    sb_append(sb, "parameters:\n");
    for (size_t i = 0; i < node->function.parameters.length; ++i) {
      Binding *param = node->function.parameters.data[i];
      print_indent_ir(sb, indent + 2);
      sb_append(sb, param->type ? param->type->name : "unknown");
      sb_append(sb, " ");
      sb_append(sb, param->name);
      sb_append(sb, "\n");
    }
    print_indent_ir(sb, indent + 1);
    sb_append(sb, "body:\n");
    print_ir_rec(node->function.block, sb, indent + 2);
    break;
  case THIR_UNARY:
    print_indent_ir(sb, indent + 1);
    sb_append(sb, "op: ");
    sb_append(sb, token_type_to_string(node->unary.op));
    sb_append(sb, "\n");
    print_indent_ir(sb, indent + 1);
    sb_append(sb, "operand:\n");
    print_ir_rec(node->unary.operand, sb, indent + 2);
    break;
  case THIR_BINARY:
    print_indent_ir(sb, indent + 1);
    sb_append(sb, "op: ");
    sb_append(sb, token_type_to_string(node->binary.op));
    sb_append(sb, "\n");
    print_indent_ir(sb, indent + 1);
    sb_append(sb, "left:\n");
    print_ir_rec(node->binary.left, sb, indent + 2);
    print_indent_ir(sb, indent + 1);
    sb_append(sb, "right:\n");
    print_ir_rec(node->binary.right, sb, indent + 2);
    break;
  case THIR_RETURN:
    print_indent_ir(sb, indent + 1);
    sb_append(sb, "value:\n");
    print_ir_rec(node->return_value, sb, indent + 2);
    break;
  case THIR_VARIABLE:
    print_indent_ir(sb, indent + 1);
    sb_append(sb, "name: ");
    sb_append(sb, node->binding ? node->binding->name : "unknown");
    sb_append(sb, "\n");
    print_indent_ir(sb, indent + 1);
    sb_append(sb, "type: ");
    sb_append(sb, node->type ? node->type->name : "unknown");
    sb_append(sb, "\n");
    break;
  case THIR_CALL:
    print_indent_ir(sb, indent + 1);
    sb_append(sb, "callee: ");
    sb_append(sb, node->call.callee ? node->call.callee->name : "unknown");
    sb_append(sb, "\n");
    print_indent_ir(sb, indent + 1);
    sb_append(sb, "arguments:\n");
    for (size_t i = 0; i < node->call.arguments.length; ++i) {
      print_ir_rec(node->call.arguments.data[i], sb, indent + 2);
    }
  }
}

static inline void print_ir(Thir *ir, String_Builder *sb) {
  print_ir_rec(ir, sb, 0);
}

#endif