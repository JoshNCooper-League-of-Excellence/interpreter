#include "ast.h"
#include "binding.h"
#include "parser.h"
#include "type.h"
#include <stddef.h>

int main(int argc, char *argv[]) {
  Context context;

  context.string_type = type_alloc(&context);
  *context.string_type = (Type){.name = "string"};

  context.integer_type = type_alloc(&context);
  *context.string_type = (Type){.name = "int"};

#if 0
  if (argc == 1) {
    fprintf(stderr, "expected filename argument. usage: bindings <filename.bi>\n");
    exit(1);
  }
  const char *filename = argv[1];
#else
  const char *filename = "input.bi";
#endif

  Ast *root = parse_file(filename, &context);

  if (!root) {
    fprintf(stderr, "no ast returned from parser\n");
    return 1;
  }

  if (root->tag == AST_ERROR) {
    fprintf(stderr, "%s at %s:%zu:%zu\n", root->error.message, filename,
            root->span.line, root->span.col);
    return 1;
  }

  return 0;
}