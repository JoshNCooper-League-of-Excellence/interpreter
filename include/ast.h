#ifndef AST_H
#define AST_H

#include "binding.h"
#include "lexer.h"
#include "list.h"
#include <stddef.h>

typedef enum {
  AST_ERROR,
  AST_PROGRAM,
  AST_LITERAL,
  AST_IDENTIFIER,
  AST_BLOCK,
  AST_FUNCTION,
  AST_UNARY,
  AST_BINARY,
  AST_RETURN,
  AST_VARIABLE,
} Ast_Tag;

typedef struct {
  const char *identifier;
  const char *type;
} Parameter;

DEFINE_LIST(Parameter);

typedef struct Ast {
  Binding *binding;
  Span span;
  Ast_Tag tag;
  size_t index;
  union {
    Ast_Ptr_list program;
    Ast_Ptr_list block;
    struct Ast *return_value;
    struct {
      const char *name;
      struct Ast *initializer;
    } variable;
    struct {
      struct Ast *left, *right;
      Token_Type op;
    } binary;

    struct {
      struct Ast *operand;
      Token_Type op;
    } unary;

    const char *identifier;
    struct {
      const char *value;
      enum {
        STRING,
        INTEGER,
      } tag;
    } literal;
    struct {
      struct Ast *block;
      Parameter_list parameters;
      const char *return_type;
      const char *name;
    } function;
    struct {
      const char *message;
      bool fatal;
    } error;
  };

} Ast;

#endif