#include "thir.h"
#include "ast.h"
#include "binding.h"
#include "lexer.h"
#include "list.h"
#include "string_builder.h"
#include "type.h"
#include "vm.h"
#include <dlfcn.h>
#include <ffi.h>

#define _GNU_SOURCE
#ifndef __USE_MISC
#define __USE_MISC
#endif

#include <stdio.h>

const char *get_function_type_string(Type_Ptr_list arguments,
                                     Type *return_type) {
  String_Builder sb = {0};
  sb_append(&sb, "function :: (");
  LIST_FOREACH(arguments, arg) {
    sb_append(&sb, arg->name);
    if (__i != arguments.length - 1) {
      sb_append(&sb, ", ");
    }
  }
  sb_appendf(&sb, ") %s;", return_type->name);
  return sb.value;
}

Binding *get_binding(const char *identifier, Span span, Context *context);

Binding_Ptr_list typer_convert_parameters(Context *context,
                                          Parameter_list parameters, Span span,
                                          Type_Ptr_list *argument_types) {
  Binding_Ptr_list bindings = {0};
  LIST_FOREACH(parameters, param) {
    Thir_Ptr thir_param = thir_alloc(context, THIR_VARIABLE, span);
    Type *param_type;

    if (!try_find_type(context, param.type, &param_type)) {
      fprintf(stderr, "unable to find type: '%s' at: %s\n", param.type,
              lexer_span_to_string(span));
      exit(1);
    }

    Binding binding = {.thir = thir_param,
                       .ast = nullptr,
                       .name = param.identifier,
                       .type = param_type};

    Binding_Ptr ptr = bind_variable(context, binding);
    thir_param->binding = ptr;
    thir_param->type = param_type;

    LIST_PTR_PUSH(argument_types, param_type);
    LIST_PUSH(bindings, ptr);
  }

  return bindings;
}

Binding *get_binding(const char *identifier, Span span, Context *context) {
  if (!identifier) {
    fprintf(stderr, "error: null identifier in get_binding()\n");
    exit(1);
  }

  LIST_FOREACH(context->ast_list, ast) {
    if (!ast->binding) {
      continue;
    }
    if (ast->tag == AST_VARIABLE) {
      if (strcmp(ast->variable.name, identifier) == 0) {
        return ast->binding;
      }
    } else if (ast->tag == AST_FUNCTION) {
      if (strcmp(ast->function.name, identifier) == 0) {
        return ast->binding;
      }
    }
  }

  LIST_FOREACH(context->thir_list, thir) {
    if (!thir->binding) {
      continue;
    }

    if (thir->tag == THIR_VARIABLE) {
      if (strcmp(thir->binding->name, identifier) == 0) {
        return thir->binding;
      }
    } else if (thir->tag == THIR_FUNCTION) {
      if (strcmp(thir->function.name, identifier) == 0) {
        return thir->binding;
      }
    } else if (thir->tag == THIR_EXTERN) {
      if (strcmp(thir->extern_function.name, identifier) == 0) {
        return thir->binding;
      }
    }
  }

  LIST_FOREACH(context->type_table, type) {
    if (!type->binding) {
      continue;
    }
    if (strcmp(type->name, identifier) == 0) {
      return type->binding;
    }
  }

  char *buf;
  char *span_string = lexer_span_to_string(span);
  asprintf(&buf, "use of undeclared identifier '%s' at: %s", identifier,
           span_string);
  fprintf(stderr, "%s\n", buf);
  exit(1);
}

[[noreturn]]
void report_error(Ast *error) {
  if (error && error->tag == AST_ERROR) {
    const char *source_range = lexer_span_to_string(error->span);
    fprintf(stderr, "Error at span %s: '%s'\n", source_range,
            error->error.message ? error->error.message : "Unknown error");
  }
  exit(1);
}

Thir *type_program(Ast *ast, Context *context) {
  Thir *program = thir_alloc(context, THIR_PROGRAM, ast->span);
  Thir_Ptr_list statements = {0};

  for (int i = 0; i < ast->program.length; ++i) {
    Ast *statement = ast->program.data[i];
    switch (statement->tag) {
    case AST_ERROR:
      report_error(statement);
      break;
    case AST_EXTERN: {
      Thir *thir = type_extern(statement, context);
      LIST_PUSH(statements, thir);
    } break;
    case AST_FUNCTION:
      Thir *thir = type_function(statement, context);
      LIST_PUSH(statements, thir);
      break;
    default:
      break;
    }
  }

  program->program = statements;
  return program;
}

Thir *type_extern(Ast *ast, Context *context) {
  Thir *$extern = thir_alloc(context, THIR_EXTERN, ast->span);

  const char *name = ast->extern_function.name;
  $extern->extern_function.name = name;

  Type_Ptr_list argument_types = {0};
  $extern->extern_function.parameters = typer_convert_parameters(
      context, ast->extern_function.parameters, ast->span, &argument_types);

  Type *return_type;
  if (!try_find_type(context, ast->extern_function.return_type, &return_type)) {
    char *buf;
    asprintf(&buf, "use of undeclared type as return type: '%s' at %s",
             ast->extern_function.return_type, lexer_span_to_string(ast->span));
    fprintf(stderr, "%s\n", buf);
    exit(1);
  }

  $extern->extern_function.return_type = return_type;

  Function_Type *type = function_type_alloc(context);
  type->base.name = get_function_type_string(argument_types, return_type);
  type->parameters = argument_types;
  type->returns = return_type;

  $extern->type = (Type *)type;
  Binding binding = {0};
  binding.ast = ast;
  binding.thir = $extern;
  binding.name = name;
  binding.type = (Type *)type;
  bind_function(context, binding, true);

  Extern_Function ffi_function = get_ffi_function_from_thir($extern);
  $extern->extern_function.index = ffi_function.index;

  return $extern;
}

Thir *type_function(Ast *ast, Context *context) {
  Thir *function = thir_alloc(context, THIR_FUNCTION, ast->span);
  typeof(ast->function) ast_fn = ast->function;
  typeof(function->function) *thir_fn = &function->function;

  thir_fn->name = ast->function.name;

  Type_Ptr_list argument_types = {0};
  thir_fn->parameters = typer_convert_parameters(context, ast_fn.parameters,
                                                 ast->span, &argument_types);

  Type *return_type;
  if (!try_find_type(context, ast_fn.return_type, &return_type)) {
    char *buf;
    asprintf(&buf, "use of undeclared type as return type: '%s' at %s",
             ast_fn.return_type, lexer_span_to_string(ast->span));
    fprintf(stderr, "%s\n", buf);
    exit(1);
  }

  thir_fn->return_type = return_type;
  context->typer_expected_type = thir_fn->return_type;
  thir_fn->block = type_block(ast_fn.block, context);
  context->typer_expected_type = nullptr;

  Function_Type *type;

  if (!try_find_function_type(context, argument_types, return_type, &type)) {
    type = function_type_alloc(context);
    type->base.name = get_function_type_string(argument_types, return_type);
    type->parameters = argument_types;
    type->returns = return_type;
  }

  function->type = (Type *)type;
  Binding binding = {0};
  binding.ast = ast;
  binding.thir = function;
  binding.name = ast->function.name;
  binding.type = (Type *)type;

  bind_function(context, binding, false);

  return function;
}

Thir *type_literal(Ast *ast, Context *context) {
  Thir *literal = thir_alloc(context, THIR_LITERAL, ast->span);

  switch (ast->literal.tag) {
  case INTEGER:
    literal->literal.value = ast->literal.value;
    literal->type = context->integer_type;
    break;
  case STRING:
    literal->literal.value = ast->literal.value;
    literal->type = context->string_type;
    break;
  }

  return literal;
}

Thir *type_identifier(Ast *ast, Context *context) {
  Binding *binding = get_binding(ast->variable.name, ast->span, context);

  if (!binding) {
    char *buf;
    asprintf(&buf, "use of undeclared identifier \"%s\" at: %s",
             ast->variable.name, lexer_span_to_string(ast->span));
    fprintf(stderr, "%s\n", buf);
    exit(1);
  }

  return binding->thir;
}

Thir *type_block(Ast *ast, Context *context) {
  Thir *block = thir_alloc(context, THIR_BLOCK, ast->span);

  Thir_Ptr_list statements = {0};
  LIST_FOREACH(ast->block, statement) {
    switch (statement->tag) {
    case AST_ERROR:
      report_error(statement);
    case AST_CALL:
      LIST_PUSH(statements, type_call(statement, context));
      break;
    case AST_UNARY:
      LIST_PUSH(statements, type_unary(statement, context));
      break;
    case AST_BINARY:
      LIST_PUSH(statements, type_binary(statement, context));
      break;
    case AST_RETURN:
      LIST_PUSH(statements, type_return(statement, context));
      break;
    case AST_VARIABLE:
      LIST_PUSH(statements, type_variable(statement, context));
      break;
    default:
      char *buf;
      asprintf(&buf, "unexpected statement type in block: %d at: %s",
               statement->tag, lexer_span_to_string(ast->span));
      fprintf(stderr, "%s\n", buf);
      exit(1);
    }
  }
  block->block = statements;

  return block;
}

Thir *type_expression(Ast *ast, Context *context) {
  switch (ast->tag) {
  case AST_CALL:
    return type_call(ast, context);
  case AST_LITERAL:
    return type_literal(ast, context);
  case AST_IDENTIFIER:
    return type_identifier(ast, context);
  case AST_UNARY:
    return type_unary(ast, context);
  case AST_BINARY:
    return type_binary(ast, context);
  default:
    char *buf;
    asprintf(&buf, "unexpected expression node type: %d, at %s", ast->tag,
             lexer_span_to_string(ast->span));
    fprintf(stderr, "%s\n", buf);
    exit(1);
  }
}

Thir *type_unary(Ast *ast, Context *context) {
  Thir *unary = thir_alloc(context, THIR_UNARY, ast->span);
  Thir *operand = type_expression(ast->unary.operand, context);

  unary->unary.op = ast->unary.op;
  unary->unary.operand = operand;
  unary->type = operand->type;
  return unary;
}

Thir *type_binary(Ast *ast, Context *context) {
  Thir *binary = thir_alloc(context, THIR_BINARY, ast->span);

  Thir *left = type_expression(ast->binary.left, context);
  Thir *right = type_expression(ast->binary.right, context);
  ;

  binary->binary.op = ast->binary.op;
  binary->binary.left = left;
  binary->binary.right = right;
  binary->type = left->type; // TODO: don't just inherit the LHS type.

  return binary;
}

Thir *type_return(Ast *ast, Context *context) {
  Thir *ret = thir_alloc(context, THIR_RETURN, ast->span);
  if (ast->return_value) {
    ret->return_value = type_expression(ast->return_value, context);
    if (context->typer_expected_type &&
        ret->return_value->type != context->typer_expected_type) {
      char *buf;
      asprintf(&buf,
               "invalid return type at: %s. \"%s\" does not match expected "
               "\"%s\"\n",
               lexer_span_to_string(ast->span), ret->return_value->type->name,
               context->typer_expected_type->name);
      fprintf(stderr, "%s\n", buf);
      exit(1);
    }
  } else if (context->typer_expected_type) {
    char *buf;
    asprintf(&buf,
             "invalid return type at: %s. this function must return a value."
             "expected: \"%s\"\n",
             lexer_span_to_string(ast->span),
             context->typer_expected_type->name);
    fprintf(stderr, "%s\n", buf);
    exit(1);
  }
  return ret;
}

Thir *type_call(Ast *ast, Context *context) {
  Binding *callee = get_binding(ast->call.callee, ast->span, context);
  Thir_Ptr_list arguments = {0};

  LIST_FOREACH(ast->call.arguments, ast_arg) {
    Thir *arg = type_expression(ast_arg, context);
    LIST_PUSH(arguments, arg);
  }

  Thir *function = callee->thir;

  int n_params = function->function.parameters.length;
  if (n_params != arguments.length) {
    char *buf;
    asprintf(&buf,
             "invalid call at %s: too %s arguments. expected %d, but got %d.",
             lexer_span_to_string(ast->span),
             n_params > arguments.length ? "few" : "many", n_params,
             arguments.length);
    fprintf(stderr, "%s\n", buf);
    exit(1);
  }

  Thir *call = thir_alloc(context, THIR_CALL, ast->span);
  call->call.arguments = arguments;
  call->call.callee = callee;

  Function_Type *callee_type = (Function_Type *)callee->type;
  call->type = callee_type->returns; // Set our type to the return type.

  return call;
}

Type *get_type_from_ast_type(Ast *ast, Context *context) {
  Type *type;
  if (!try_find_type(context, ast->type.path, &type)) {
    char *buf;
    asprintf(&buf, "undeclared type: '%s' at: %s", ast->type.path,
             lexer_span_to_string(ast->span));
    fprintf(stderr, "%s\n", buf);
    exit(1);
  }
  return type;
}

Thir *type_variable(Ast *ast, Context *context) {
  Thir *var = thir_alloc(context, THIR_VARIABLE, ast->span);

  Thir *initializer = nullptr;

  if (ast->variable.initializer) {
    initializer = type_expression(ast->variable.initializer, context);
  } else {
    char *buf;
    asprintf(&buf, "uninitialized variables not allowed: %d, at: %s",
             ast->variable.initializer->tag,
             lexer_span_to_string(ast->variable.initializer->span));
    fprintf(stderr, "%s\n", buf);
    exit(1);
  }

  if (!ast->variable.type) {
    char *buf;
    asprintf(&buf, "un-typed variables not allowed: %d, at: %s",
             ast->variable.initializer->tag,
             lexer_span_to_string(ast->variable.initializer->span));
    fprintf(stderr, "%s\n", buf);
    exit(1);
  }

  Type *type = get_type_from_ast_type(ast->variable.type, context);

  if (initializer->type != type) {
    char *buf;
    asprintf(
        &buf, "invalid type in declaration at: %s, expected %s, but got %s.",
        lexer_span_to_string(ast->span), type->name, initializer->type->name);
    fprintf(stderr, "%s\n", buf);
    exit(1);
  }

  Binding binding = {0};
  binding.ast = ast;
  binding.thir = var;
  binding.name = ast->variable.name;
  binding.type = initializer->type;

  // TODO: this variable system needs work, we have to do some backflips
  bind_variable(context, binding);

  var->type = initializer->type;
  var->variable_initializer = initializer;

  return var;
}

ffi_type type_to_ffi_type(Type *type) {
  switch (type->tag) {
  case TYPE_INT:
    return ffi_type_sint32;
  case TYPE_STRING:
    return ffi_type_pointer;
  case TYPE_VOID:
    return ffi_type_void;
  default:
    fprintf(stderr, "unable to use type: %s with libffi\n", type->name);
    exit(1);
  }
}

Extern_Function get_ffi_function_from_thir(Thir *thir) {
  LIST_FOREACH(CACHED_EXTERNS, cached) {
    if (cached.name == thir->extern_function.name) {
      return cached;
    }
  }

  const char *LIB_SEARCH_PATHS[] = {"libm.so.6",
                                    "libm.so",
                                    "libc.so.6",
                                    "libc.so",
                                    "/home/josh/source/c/bindings/libb.so",
                                    NULL};

  static struct {
    const char *name;
    void *handle;
  } open_libs[16] = {};
  static int open_libs_count = 0;

  void *handle = NULL;
  void *symbol = NULL;

  for (const char **p = LIB_SEARCH_PATHS; *p != NULL; ++p) {
    int found = 0;
    for (int i = 0; i < open_libs_count; ++i) {
      if (strcmp(open_libs[i].name, *p) == 0) {
        handle = open_libs[i].handle;
        found = 1;
        break;
      }
    }
    if (!found) {
      handle = dlopen(*p, RTLD_NOW);
      if (handle) {

        if (open_libs_count < (int)(sizeof(open_libs) / sizeof(open_libs[0]))) {
          open_libs[open_libs_count].name = *p;
          open_libs[open_libs_count].handle = handle;
          ++open_libs_count;
        } else {
          fprintf(stderr, "unable to open dynamic library for ffi: too many "
                          "libraries open\n");
          exit(1);
        }
      }
    }

    if (!handle) {
      continue;
    }

    dlerror();
    symbol = dlsym(handle, thir->extern_function.name);

    if (symbol) {
      break;
    }
  }

  if (!symbol) {
    fprintf(stderr, "unable to find symbol '%s' in system libraries\n",
            thir->extern_function.name);
    exit(1);
  }

  Function_Type *type = (Function_Type *)thir->type;

  Extern_Function extern_function = {
      .name = thir->extern_function.name,
      .parameters = {0},
      .return_type = type_to_ffi_type(thir->function.return_type),
      .index = CACHED_EXTERNS.length,
      .ptr = symbol,
      .original_return_type = type->returns};

  LIST_FOREACH(thir->extern_function.parameters, parameter) {
    LIST_PUSH(extern_function.parameters, type_to_ffi_type(parameter->type));
  }

  LIST_PUSH(CACHED_EXTERNS, extern_function);

  return extern_function;
}

Value libffi_dynamic_dispatch(Extern_Function function, Value *argv, int argc) {
  ffi_cif cif;

  if (!function.ptr) {
    fprintf(stderr, "[VM:FFI] error: function.ptr is (nil) for extern '%s'\n",
            function.name);
    exit(1);
  }

  size_t n_params = function.parameters.length;
  ffi_type *arg_types[n_params ? n_params : 1];
  void *arg_values[n_params ? n_params : 1];

  if ((size_t)argc < n_params) {
    fprintf(stderr,
            "[VM:FFI] attempted to call \"%s\" via 'extern', but too few "
            "arguments were "
            "provided. got=%d need=%zu\n",
            function.name, argc, n_params);
    exit(1);
  }

  for (size_t i = 0; i < n_params; ++i) {
    arg_types[i] = &function.parameters.data[i];
    Value *v = &argv[i];
    switch (v->type) {
    case VALUE_INTEGER:
      arg_values[i] = &v->integer;
      break;
    case VALUE_STRING:
      arg_values[i] = &v->string;
      break;
    default:
      fprintf(stderr, "[VM:FFI] unsupported argument type for extern call\n");
      exit(1);
    }
  }

  ffi_type *ffi_return_type = &function.return_type;

  int int_buf = 0;
  char *string_buf = NULL;
  void *return_buf = NULL;

  int return_type = function.original_return_type->tag;

  if (return_type == TYPE_INT) {
    return_buf = &int_buf;
  } else if (return_type == TYPE_STRING) {
    return_buf = &string_buf;
  } else if (return_type == TYPE_VOID) {
    return_buf = NULL;
  }

  int prep = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, (unsigned)n_params,
                          ffi_return_type, arg_types);
  if (prep != FFI_OK) {
    fprintf(stderr, "[VM:FFI] ffi_prep_cif failed: %d\n", prep);
    exit(1);
  }

  ffi_call(&cif, function.ptr, return_buf, arg_values);

  if (return_type == TYPE_INT) {
    return (Value){.type = VALUE_INTEGER, .integer = int_buf};
  } else if (return_type == TYPE_STRING) {
    return (Value){.type = VALUE_STRING, .string = string_buf};
  } else if (return_type == TYPE_VOID) {
    return (Value){.type = VALUE_VOID};
  }

  fprintf(stderr, "[VM:FFI] no return type was specified\n");
  exit(1);
}
