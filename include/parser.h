#include "binding.h"
#include "lexer.h"
#include "ast.h"

Ast *parse_file(const char *filename, Context *context);

Ast *parse_program(Lexer *lexer, Context *context);

Ast *parse_literal(Lexer *lexer, Context *context);
Ast *parse_primary(Lexer *lexer, Context *context);
Ast *parse_binary(Lexer *lexer, Context *context);

Ast *parse_function(Lexer *lexer, Context *context);
Ast *parse_block(Lexer *lexer, Context *context);
