#include "binding.h"
#include "ast.h"

Ast *ast_alloc(Context *context, int tag, Span span) {
  struct Ast *ast = calloc(1, sizeof(struct Ast));
  ast->index = context->asts.length;
  ast->span = span;
  ast->tag = tag;
  LIST_PUSH(context->asts, ast);
  return ast;
}