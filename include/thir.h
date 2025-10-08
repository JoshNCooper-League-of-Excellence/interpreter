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
} Thir_Tag;

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
Thir *type_unary(struct Ast *, Context *context);
Thir *type_binary(struct Ast *, Context *context);
Thir *type_return(struct Ast *, Context *context);
Thir *type_variable(struct Ast *, Context *context);

#endif