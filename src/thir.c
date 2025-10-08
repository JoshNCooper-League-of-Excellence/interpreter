#include "thir.h"
#include "ast.h"
#include "binding.h"
#include "lexer.h"
#include "list.h"

Binding *get_binding(const char *identifier, Span span, Context *context);

Binding_Ptr_list typer_convert_parameters(Parameter_list parameters, Span span,
                                          Context *context) {
  Binding_Ptr_list bindings = {0};
  LIST_FOREACH(parameters, param) {
    Thir_Ptr thir_param = thir_alloc(context, THIR_VARIABLE, span);
    Type *param_type;

    if (!try_find_type(context, param.type, &param_type)) {
      fprintf(stderr, "unable to find type: '%s' at: %s\n", param.type,
              lexer_span_to_string(&span));
      exit(1);
    }

    Binding *binding =
        binding_alloc(context, (Binding){.thir = thir_param,
                                         .ast = nullptr,
                                         .name = param.identifier,
                                         .type = param_type});

    LIST_PUSH(bindings, binding);
  }

  return bindings;
}

Binding *get_binding(const char *identifier, Span span, Context *context) {
  if (!identifier) {
    fprintf(stderr, "error: null identifier in get_binding()\n");
    exit(1);
  }
  LIST_FOREACH(context->ast_list, ast) {
    if (!ast) {
      fprintf(stderr, "error: null Ast in get_binding()\n");
      exit(1);
    }
    if (ast->tag == AST_VARIABLE) {
      if (strcmp(ast->variable.name, identifier) == 0) {
        return ast->binding;
      }
    } else if (ast->tag == AST_FUNCTION) {
      if (strcmp(ast->function.name, identifier)) {
        return ast->binding;
      }
    }
  }

  LIST_FOREACH(context->thir_list, thir) {
    if (thir->tag == THIR_VARIABLE) {
      if (thir->binding && strcmp(thir->binding->name, identifier) == 0) {
        return thir->binding;
      }
    } else if (thir->tag == THIR_FUNCTION) {
      if (strcmp(thir->function.name, identifier)) {
        return thir->binding;
      }
    }
  }

  LIST_FOREACH(context->type_table, type) {
    if (strcmp(type->name, identifier) == 0) {
      return type->binding;
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

  thir_fn->block = type_block(ast_fn.block, context);

  thir_fn->parameters =
      typer_convert_parameters(ast_fn.parameters, ast->span, context);

  Type *return_type;
  if (!try_find_type(context, ast_fn.return_type, &return_type)) {
    char *buf;
    asprintf(&buf, "use of undeclared type as return type: '%s' at %s",
             ast_fn.return_type, lexer_span_to_string(&ast->span));
    fprintf(stderr, "%s\n", buf);
    exit(1);
  }

  thir_fn->return_type = return_type;

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
  Thir *variable = thir_alloc(context, THIR_VARIABLE, ast->span);
  variable->binding = get_binding(ast->variable.name, ast->span, context);
  return variable;
}

Thir *type_block(Ast *ast, Context *context) {
  Thir *block = thir_alloc(context, THIR_BLOCK, ast->span);

  Thir_Ptr_list statements = {0};
  LIST_FOREACH(ast->block, statement) {
    switch (statement->tag) {
    case AST_ERROR:
      report_error(statement);
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

  return block;
}

Thir *type_unary(Ast *ast, Context *context) {
  Thir *unary = thir_alloc(context, THIR_UNARY, ast->span);

  Thir *operand = nullptr;
  switch (ast->unary.operand->tag) {
  case AST_LITERAL:
    operand = type_literal(ast->unary.operand, context);
    break;
  case AST_IDENTIFIER:
    operand = type_identifier(ast->unary.operand, context);
    break;
  case AST_UNARY:
    operand = type_unary(ast->unary.operand, context);
    break;
  case AST_BINARY:
    operand = type_binary(ast->unary.operand, context);
    break;
  default:
    char *buf;
    asprintf(&buf, "unexpected node in unary expression: %d, at: %s\n",
             ast->unary.operand->tag, lexer_span_to_string(&ast->span));
    fprintf(stderr, "%s\n", buf);
    exit(1);
  }

  unary->unary.op = ast->unary.op;
  unary->unary.operand = operand;
  unary->type = operand->type;

  return unary;
}

Thir *type_binary(Ast *ast, Context *context) {
  Thir *binary = thir_alloc(context, THIR_BINARY, ast->span);

  Thir *left = nullptr;
  Thir *right = nullptr;

  switch (ast->binary.left->tag) {
  case AST_LITERAL:
    left = type_literal(ast->binary.left, context);
    break;
  case AST_IDENTIFIER:
    left = type_identifier(ast->binary.left, context);
    break;
  case AST_UNARY:
    left = type_unary(ast->binary.left, context);
    break;
  case AST_BINARY:
    left = type_binary(ast->binary.left, context);
    break;
  default:
    fprintf(stderr, "unexpected node in binary left operand: %d, at: %s\n",
            ast->binary.left->tag, lexer_span_to_string(&ast->span));
    exit(1);
  }

  switch (ast->binary.right->tag) {
  case AST_LITERAL:
    right = type_literal(ast->binary.right, context);
    break;
  case AST_IDENTIFIER:
    right = type_identifier(ast->binary.right, context);
    break;
  case AST_UNARY:
    right = type_unary(ast->binary.right, context);
    break;
  case AST_BINARY:
    right = type_binary(ast->binary.right, context);
    break;
  default:
    fprintf(stderr, "unexpected node in binary right operand: %d, at: %s\n",
            ast->binary.right->tag, lexer_span_to_string(&ast->span));
    exit(1);
  }

  binary->binary.op = ast->binary.op;
  binary->binary.left = left;
  binary->binary.right = right;
  binary->type = left->type; // TODO: don't just inherit the LHS type.

  return binary;
}

Thir *type_return(Ast *ast, Context *context) {
  Thir *ret = thir_alloc(context, THIR_RETURN, ast->span);
  if (ast->return_value) {
    switch (ast->return_value->tag) {
    case AST_LITERAL:
      ret->return_value = type_literal(ast->return_value, context);
      break;
    case AST_IDENTIFIER:
      ret->return_value = type_identifier(ast->return_value, context);
      break;
    case AST_UNARY:
      ret->return_value = type_unary(ast->return_value, context);
      break;
    case AST_BINARY:
      ret->return_value = type_binary(ast->return_value, context);
      break;
    default:
      char *buf;
      asprintf(&buf, "unexpected return value expression node type: %d, at %s",
               ast->return_value->tag,
               lexer_span_to_string(&ast->return_value->span));
      fprintf(stderr, "%s\n", buf);
      exit(1);
    }
  }
  return ret;
}

Thir *type_variable(Ast *ast, Context *context) {
  Thir *var = thir_alloc(context, THIR_VARIABLE, ast->span);

  Binding binding = {0};
  binding.ast = ast;
  binding.thir = var;
  binding.name = ast->variable.name;

  Thir *initializer = nullptr;
  if (ast->variable.initializer) {
    switch (ast->variable.initializer->tag) {
    case AST_LITERAL:
      initializer = type_literal(ast->variable.initializer, context);
      break;
    case AST_IDENTIFIER:
      initializer = type_identifier(ast->variable.initializer, context);
      break;
    case AST_UNARY:
      initializer = type_unary(ast->variable.initializer, context);
      break;
    case AST_BINARY:
      initializer = type_binary(ast->variable.initializer, context);
      break;
    default:
      char *buf;
      asprintf(&buf, "unexpected node type in variable initializer: %d, at: %s",
               ast->variable.initializer->tag,
               lexer_span_to_string(&ast->variable.initializer->span));
      fprintf(stderr, "%s\n", buf);
      exit(1);
    }
  } else {
    char *buf;
    asprintf(&buf, "uninitialized variables not allowed: %d, at: %s",
             ast->variable.initializer->tag,
             lexer_span_to_string(&ast->variable.initializer->span));
    fprintf(stderr, "%s\n", buf);
    exit(1);
  }

  binding.type = initializer->type;
  binding_alloc(context, binding);

  return var;
}
