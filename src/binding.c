#include "binding.h"
#include "ast.h"

Ast_Ptr ast_alloc(Context *context, int tag) {
  struct Ast *ast = malloc(sizeof(struct Ast));
  ast->index = context->asts.length;
  ast->tag = tag;
  LIST_PUSH(context->asts, ast);
  return ast;
}