#include "parser.h"
#include "lexer.h"

Ast *parse_file(const char *filename, Context *context) {
  Lexer lexer;
  lexer_init(&lexer, filename);
  return parse_program(&lexer, context);
}

Ast *parse_program(Lexer *lexer, Context *context) {
  return nullptr;
}
Ast *parse_literal(Lexer *lexer, Context *context) {
  return nullptr;
}
Ast *parse_primary(Lexer *lexer, Context *context) {
  return nullptr;
}
Ast *parse_binary(Lexer *lexer, Context *context) {
  return nullptr;
}
Ast *parse_function(Lexer *lexer, Context *context) {
  return nullptr;
}
Ast *parse_block(Lexer *lexer, Context *context) {
  return nullptr;
}