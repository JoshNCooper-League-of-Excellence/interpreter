#include "ast.h"
#include "binding.h"
#include "lexer.h"
#include "parser.h"
#include "string_builder.h"
#include "ir.h"
#include "thir.h"
#include "vm.h"
#include <stddef.h>
#include <string.h>

const char *CURRENTLY_COMPILING_FILE_NAME = "<no filename>";
Extern_Function_list CACHED_EXTERNS;

bool LOG_IR = false;

Cmd_Line_Args COMMAND_LINE_ARGUMENTS;


int main(int argc, char *argv[]) {
  if (argc == 1) {
    fprintf(stderr, "expected filename argument. usage: ./<compiler> <filename.b>\n");
    exit(1);
  }

  COMMAND_LINE_ARGUMENTS = (Cmd_Line_Args){.argc = argc - 1, .argv = argv + 1};

  const char *output_path = nullptr;
  for (int i = 1; i < argc; ++i) {
    const char *arg = argv[i];
    if (strncmp(arg, "-log-ir", 7) == 0) {
      LOG_IR = true;
    } else if (strncmp(arg, "-o=", 3) == 0) {
      output_path = arg + 3;
    }
  }

  CACHED_EXTERNS = (Extern_Function_list){0};

  Context context = {0};
  context_initialize(&context);

  const char *filename = argv[1];
  CURRENTLY_COMPILING_FILE_NAME = filename;

  Ast *ast_program = parse_file(filename, &context);
  if (!ast_program) {
    fprintf(stderr, "no ast returned from parser\n");
    return 1;
  }

  if (ast_program->tag == AST_ERROR) {
    fprintf(stderr, "%s at %s:%zu:%zu\n", ast_program->error.message, filename, ast_program->span.line,
            ast_program->span.col);
    return 1;
  }

  Thir *typed_program = type_program(ast_program, &context);

  Module module = {0};
  module_init(&module, &context);
  lower_program(typed_program, &module);

  if (LOG_IR || output_path) {
    String_Builder sb = {0};
    print_module(&module, &sb);
    if (LOG_IR) {
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

  vm_execute(&module);

  return 0;
}