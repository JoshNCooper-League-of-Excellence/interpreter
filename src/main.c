#include "ast.h"
#include "binding.h"
#include "lexer.h"
#include "list.h"
#include "parser.h"
#include "tac.h"
#include "thir.h"
#include "type.h"
#include <stddef.h>

const char *CURRENTLY_COMPILING_FILE_NAME = "<no filename>";

int main(int argc, char *argv[]) {
  Context context;

  context.string_type = type_alloc(&context);
  context.string_type->name = "string";

  context.integer_type = type_alloc(&context);
  context.integer_type->name = "int";

#if 0
  if (argc == 1) {
    fprintf(stderr, "expected filename argument. usage: bindings <filename.bi>\n");
    exit(1);
  }
  const char *filename = argv[1];
#else
  const char *filename = "input.bi";
  #endif
  CURRENTLY_COMPILING_FILE_NAME = filename;

  Ast *ast_program = parse_file(filename, &context);

  if (!ast_program) {
    fprintf(stderr, "no ast returned from parser\n");
    return 1;
  }

  if (ast_program->tag == AST_ERROR) {
    fprintf(stderr, "%s at %s:%zu:%zu\n", ast_program->error.message, filename,
            ast_program->span.line, ast_program->span.col);
    return 1;
  }

  Thir *typed_program = type_program(ast_program, &context);

  LIST_FOREACH(context.bindings, binding) {
    printf("binding {\n");
    if (binding->name) {
      printf("\tname: '%s'\n", binding->name);
    } 
    if (binding->ast) {
      printf("\tAst: %p\n", binding->ast);
    }
    if (binding->thir) {
      printf("\tThir: %p\n", binding->thir);
    }
    if (binding->type) {
      printf("\ttype: %p aka %s\n", binding->type, binding->type->name);
    }
    printf("}\n");
  }

  Module module = {0};
  lower_program(typed_program, &module);

  String_Builder sb = {0};
  print_module(&module, &sb);

  printf("%s\n", sb.value);

  return 0;
}