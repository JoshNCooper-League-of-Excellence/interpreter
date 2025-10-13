#include <linux/limits.h>
#define _GNU_SOURCE

#include "ast.h"
#include "binding.h"
#include "ir.h"
#include "lexer.h"
#include "parser.h"
#include "string_builder.h"
#include "thir.h"
#include "vm.h"
#include <limits.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

char CURRENTLY_COMPILING_FILE_NAME[PATH_MAX];

Extern_Function_list CACHED_EXTERNS;
Cmd_Line_Args COMMAND_LINE_ARGUMENTS;
bool LOG_IR_TO_STDOUT = false;
bool DO_RUN_WHEN_COMPILED = false;

int main(int argc, char *argv[]) {
  if (argc == 1) {
    fprintf(stderr, "expected filename argument. usage: ./<compiler> <filename.b>\n");
    exit(1);
  }

  
  CACHED_EXTERNS = (Extern_Function_list){0};
  COMMAND_LINE_ARGUMENTS = (Cmd_Line_Args){.argc = argc - 1, .argv = argv + 1};

  const char *output_path = nullptr;
  for (int i = 1; i < argc; ++i) {
    const char *arg = argv[i];
    if (strncmp(arg, "-log-ir", 7) == 0) {
      LOG_IR_TO_STDOUT = true;
    } else if (strncmp(arg, "-o=", 3) == 0) {
      output_path = arg + 3;
    } else if (strncmp(arg, "-run", 3) == 0) {
      DO_RUN_WHEN_COMPILED = true;
    } else if (strstr(arg, ".q")) {
      realpath(arg, CURRENTLY_COMPILING_FILE_NAME);
    }
  }

  Context context = {0};
  context_initialize(&context);

  Ast *ast_program = parse_file(CURRENTLY_COMPILING_FILE_NAME, &context);
  if (!ast_program) {
    fprintf(stderr, "no ast returned from parser\n");
    return 1;
  }

  if (ast_program->tag == AST_ERROR) {
    fprintf(stderr, "%s at %s:%zu:%zu\n", ast_program->error.message, CURRENTLY_COMPILING_FILE_NAME, ast_program->span.line,
            ast_program->span.col);
    return 1;
  }

  Thir *typed_program = type_program(ast_program, &context);

  Module module = {0};
  module_init(&module, &context);
  IR_Context ir_context = {0};
  lower_program(typed_program, &module, &ir_context);

  if (LOG_IR_TO_STDOUT || output_path) {
    String_Builder sb = {0};
    print_module(&module, &sb);
    if (LOG_IR_TO_STDOUT) {
      printf("%s\n", sb.value);
    } else {
      FILE *file = fopen(output_path, "w");
      if ((file = fopen(output_path, "w"))) {
        fwrite(sb.value, 1, sb.length, file);
        fclose(file);
      } else {
        fprintf(stderr, "unable to open file %s to write IR.\n", output_path);
        exit(1);
      }
    }
  }

  if (DO_RUN_WHEN_COMPILED) {
    vm_execute(&module);
  }

  return 0;
}