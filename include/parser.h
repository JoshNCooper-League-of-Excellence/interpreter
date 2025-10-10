#include "ast.h"
#include "binding.h"
#include "lexer.h"
#include <limits.h>

#define RETURN_OP_NONE_MSG(msg)                                                \
  do {                                                                         \
    if (error && !*error) {                                                    \
      *error = parser_error(context, tok.span, (msg));                         \
    }                                                                          \
    return OPERATOR_NONE;                                                      \
  } while (0)

#define RETURN_OP_NONE(kind)                                                   \
  do {                                                                         \
    char *buf;                                                                 \
    asprintf(&buf, "unexpected %s operator token: %s", (kind),                 \
             token_type_to_string(tok.type));                                  \
    if (error && !*error)                                                      \
      *error = parser_error(context, tok.span, buf);                           \
    return OPERATOR_NONE;                                                      \
  } while (0)

Operator parse_operator(Lexer *lexer, Expression_Type expr_type);

Precedence get_precedence(Operator op, bool *is_valid_operator);

Ast *parse_file(const char *filename, Context *context);

Ast *parse_program(Lexer *lexer, Context *context);

Ast *parse_primary(Lexer *lexer, Context *context);
Ast *parse_postfix(Lexer *lexer, Context *context);
Ast *parse_binary(Lexer *lexer, Context *context, Precedence precedence);
Ast *parse_expression(Lexer *lexer, Context *context);

Ast *parse_aggregate_initializer(Lexer *lexer, Context *context);

Ast *parse_extern(Lexer *lexer, Context *context);
Ast *parse_identifier(Lexer *lexer, Context *context);
Ast *parse_if(Lexer *lexer, Context *context);
Ast *parse_struct(Lexer *lexer, Context *context);
Ast *parse_function(Lexer *lexer, Context *context);
Ast *parse_block(Lexer *lexer, Context *context);
Ast *parse_type(Lexer *lexer, Context *context);
Ast *parse_variable(Lexer *lexer, Context *context);