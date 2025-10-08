#include "ast.h"
#include "binding.h"
#include "lexer.h"

typedef enum {
  PREC_NONE = 0,
  PREC_ASSIGNMENT, // =
  PREC_TERM,       // + -
  PREC_FACTOR,     // * /
  PREC_CALL,       // ()
  PREC_PRIMARY,    // literals, identifiers
} Precedence;

static inline Precedence get_precedence(Token_Type type) {
  switch (type) {
  case TOKEN_ASSIGN:
    return PREC_ASSIGNMENT;
  case TOKEN_PLUS:
  case TOKEN_MINUS:
    return PREC_TERM;
  case TOKEN_STAR:
  case TOKEN_SLASH:
    return PREC_FACTOR;
  case TOKEN_LPAREN:
    return PREC_CALL;
  case TOKEN_IDENTIFIER:
  case TOKEN_INTEGER:
  case TOKEN_STRING:
    return PREC_PRIMARY;
  default:
    return PREC_NONE;
  }
}

Ast *parse_file(const char *filename, Context *context);

Ast *parse_program(Lexer *lexer, Context *context);

Ast *parse_primary(Lexer *lexer, Context *context);
Ast *parse_binary(Lexer *lexer, Context *context, Precedence precedence);
Ast *parse_expression(Lexer *lexer, Context *context);

Ast *parse_function(Lexer *lexer, Context *context);
Ast *parse_block(Lexer *lexer, Context *context);
Ast *parse_variable(Lexer *lexer, Context *context);