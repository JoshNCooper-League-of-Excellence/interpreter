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
#include <string.h>

const char *CURRENTLY_COMPILING_FILE_NAME = "<no filename>";
Extern_Function_list CACHED_EXTERNS;

bool LOG_AST = false;
bool LOG_THIR = false;
bool LOG_BINDINGS = false;
bool LOG_TAC = false;
bool LOG_MAX = false;


Cmd_Line_Args COMMAND_LINE_ARGUMENTS;

static void set_log_flag(const char *level) {
  if (!level || !*level)
    return;
  if (!strcmp(level, "ast")) {
    LOG_AST = true;
    return;
  }
  if (!strcmp(level, "thir")) {
    LOG_THIR = true;
    return;
  }
  if (!strcmp(level, "ir")) {
    LOG_TAC = true;
    return;
  }
  if (!strcmp(level, "all") || !strcmp(level, "max")) {
    LOG_AST = LOG_THIR = LOG_BINDINGS = LOG_TAC = LOG_MAX = true;
    return;
  }
  if (!strcmp(level, "none")) {
    return;
  } // default is false; do nothing
}

static void set_log_flags_from_arg(const char *arg) {
  const char *p = arg;
  while (*p) {
    const char *slash = strchr(p, '/');
    if (!slash) {
      set_log_flag(p);
      break;
    } else {
      size_t len = (size_t)(slash - p);
      char buf[32];
      if (len >= sizeof(buf))
        len = sizeof(buf) - 1;
      memcpy(buf, p, len);
      buf[len] = 0;
      set_log_flag(buf);
      p = slash + 1;
    }
  }
}

int main(int argc, char *argv[]) {
  COMMAND_LINE_ARGUMENTS = (Cmd_Line_Args){.argc = argc - 1, .argv = argv + 1};

  const char *output_path = nullptr;
  for (int i = 1; i < argc; ++i) {
    const char *arg = argv[i];
    if (strncmp(arg, "-log=", 5) == 0) {
      set_log_flags_from_arg(arg + 5);
    } else if (strncmp(arg, "-o=", 3) == 0) {
      output_path = arg + 3;
    }
  }

  CACHED_EXTERNS = (Extern_Function_list){0};

  Context context = {0};
  context_initialize(&context);

#if !DEBUG
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
  if (LOG_AST) {
    print_ast(ast_program, &sb);
    printf("%s\n", sb.value);
    sb_free(&sb);
  }

  Thir *typed_program = type_program(ast_program, &context);

  if (LOG_THIR) {
    print_ir(typed_program, &sb);
    printf("%s\n", sb.value);
    sb_free(&sb);
  }

  if (LOG_BINDINGS) {
    LIST_FOREACH(context.bindings, binding) {
      printf("binding {\n");
      if (binding->name) {
        printf("    name: '%s'\n", binding->name);
      }
      if (binding->ast) {
        printf("    Ast: %p\n", binding->ast);
      }
      if (binding->thir) {
        printf("    Thir: %p\n", binding->thir);
      }
      if (binding->type) {
        printf("    type: %p aka %s\n", binding->type, binding->type->name);
      }
      printf("}\n");
    }
    printf("%s\n", sb.value);
    sb_free(&sb);
  }

  Module module = {0};
  module_init(&module, &context);
  lower_program(typed_program, &module);

  if (LOG_TAC || output_path) {
    String_Builder sb = {0};
    print_module(&module, &sb);
    if (LOG_TAC) {
      printf("%s\n", sb.value);
    } else {
      FILE *file = fopen(output_path, "w");
      if ((file = fopen(output_path, "w"))) {
        fwrite(sb.value, 1, sb.length, file);
        fclose(file);
      } else {
        fprintf(stderr, "unable to open file %s to write IR.\n",
                output_path);
        exit(1);
      }
    }
  }

  vm_execute(&module);

  return 0;
}