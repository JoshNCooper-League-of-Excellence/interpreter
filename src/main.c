#include <stddef.h>
#include "binding.h"
#include "ast.h"
#include "parser.h"
#include "type.h"

int main(int argc, char *argv[]) {
  Context context;

  context.string_type = type_alloc(&context);
  *context.string_type = (Type) {
    .name = "string"
  };

  context.integer_type = type_alloc(&context);
  *context.string_type = (Type) {
    .name = "int"
  };

  if (argc == 1) {
    fprintf(stderr, "expected filename argument. usage: bindings <filename.bi>\n");
    exit(1);
  }

  Ast *root = parse_file(argv[1], &context);

  return 0;
}