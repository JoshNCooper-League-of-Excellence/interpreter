#ifndef AST_H
#define AST_H

#include "binding.h"
#include "lexer.h"
#include "list.h"

typedef enum {
  AST_LITERAL,
  AST_IDENTIFIER,
  AST_BLOCK,
  AST_FUNCTION,
} Ast_Tag;

typedef struct {
  Binding identifier;
  Binding type;
} Parameter; 

DEFINE_LIST(Parameter);

typedef struct Ast {
  Span span;
  Ast_Tag tag;
  union {
    struct {
      Token literal;
      enum {
        STRING,
        INTEGER,
      } literal_tag;
    };
    Binding identifier;
    struct {
      struct Ast *block;
      Parameter_list parameters;
      Binding return_type;
    };
  };

} Ast;

#endif