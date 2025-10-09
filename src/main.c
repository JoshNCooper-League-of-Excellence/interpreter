#include "ast.h"
#include "binding.h"
#include "lexer.h"
#include "list.h"
#include "parser.h"
#include "string_builder.h"
#include "tac.h"
#include "thir.h"
#include "type.h"
#include "vm.h"
#include <stddef.h>

const char *CURRENTLY_COMPILING_FILE_NAME = "<no filename>";
Extern_Function_list CACHED_EXTERNS;

#define LOG_LEVEL 0
#define NO_LOGS 0
#define LOG_AST 1
#define LOG_THIR 2
#define LOG_BINDINGS 3
#define LOG_TAC 4
#define LOG_MAX 4


Cmd_Line_Args COMMAND_LINE_ARGUMENTS;

int main(int argc, char *argv[]) {
  COMMAND_LINE_ARGUMENTS = (Cmd_Line_Args){
    .argc = argc - 1,
    .argv = argv + 1
  };

  CACHED_EXTERNS = (Extern_Function_list){0};

  Context context = {0};

  context.string_type = type_alloc(&context);
  context.string_type->name = "string";
  context.string_type->tag = TYPE_STRING;

  context.integer_type = type_alloc(&context);
  context.integer_type->name = "int";
  context.integer_type->tag = TYPE_INT;

  Type *void_type = type_alloc(&context);
  void_type->name = "void";
  void_type->tag = TYPE_VOID;

#if 0
  if (argc == 1) {
    fprintf(stderr, "expected filename argument. usage: bindings <filename.bi>\n");
    exit(1);
  }
  const char *filename = argv[1];
#else
  const char *filename = "main.b";
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

  String_Builder sb = {0};
  if (LOG_LEVEL >= LOG_AST) {
    print_ast(ast_program, &sb);
  }

  Thir *typed_program = type_program(ast_program, &context);

  if (LOG_LEVEL >= LOG_THIR) {
    print_ir(typed_program, &sb);
  }

  if (LOG_LEVEL >= LOG_BINDINGS) {
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
  }

  Module module = {0};
  lower_program(typed_program, &module);

  if (LOG_LEVEL >= LOG_TAC) {
    print_module(&module, &sb);
    printf("%s\n", sb.value);
  }

  if (LOG_LEVEL >= LOG_TAC) {
    sb_free(&sb);
  }

  vm_execute(&module);

  return 0;
}