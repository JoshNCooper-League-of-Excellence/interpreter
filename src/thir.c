#include "thir.h"
#include "ast.h"
#include "binding.h"
#include "lexer.h"
#include "list.h"
#include "string_builder.h"
#include "type.h"
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
              lexer_span_to_string(&span));
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

  LIST_FOREACH(context->bindings, binding) {
    if (strcmp(binding->name, identifier) == 0 && binding->thir) {
      return binding;
    }
  }

  char *buf;
  char *span_string = lexer_span_to_string(&span);
  asprintf(&buf, "use of undeclared identifier '%s' at: %s", identifier,
           span_string);
  fprintf(stderr, "%s\n", buf);
  exit(1);
}

[[noreturn]]
void report_error(Ast *error) {
  if (error && error->tag == AST_ERROR) {
    const char *source_range = lexer_span_to_string(&error->span);
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

Thir *type_function(Ast *ast, Context *context) {
  Thir *function = thir_alloc(context, THIR_FUNCTION, ast->span);
  typeof(ast->function) ast_fn = ast->function;
  typeof(function->function) *thir_fn = &function->function;

  thir_fn->name = ast->function.name;

  Type_Ptr_list argument_types = {0};
  thir_fn->parameters = typer_convert_parameters(context, ast_fn.parameters,
                                                 ast->span, &argument_types);

  thir_fn->block = type_block(ast_fn.block, context);

  Type *return_type;
  if (!try_find_type(context, ast_fn.return_type, &return_type)) {
    char *buf;
    asprintf(&buf, "use of undeclared type as return type: '%s' at %s",
             ast_fn.return_type, lexer_span_to_string(&ast->span));
    fprintf(stderr, "%s\n", buf);
    exit(1);
  }

  thir_fn->return_type = return_type;

  Function_Type *type = function_type_alloc(context);
  type->base.name = get_function_type_string(argument_types, return_type);
  type->parameters = argument_types;
  type->returns = return_type;

  function->type = (Type *)type;
  Binding binding = {0};
  binding.ast = ast;
  binding.thir = function;
  binding.name = ast->function.name;
  binding.type = (Type *)type;

  bind_function(context, binding);

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
             ast->variable.name, lexer_span_to_string(&ast->span));
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
               statement->tag, lexer_span_to_string(&ast->span));
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
             lexer_span_to_string(&ast->span));
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

  Thir *call = thir_alloc(context, THIR_CALL, ast->span);
  call->call.arguments = arguments;
  call->call.callee = callee;

  Function_Type *callee_type = (Function_Type *)callee->type;
  call->type = callee_type->returns; // Set our type to the return type.

  return call;
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
             lexer_span_to_string(&ast->variable.initializer->span));
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
