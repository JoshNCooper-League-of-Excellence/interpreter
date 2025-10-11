#include "thir.h"
#include "ast.h"
#include "binding.h"
#include "core.h"
#include "lexer.h"
#include "list.h"
#include "string_builder.h"
#include "type.h"
#include <dlfcn.h>
#include <ffi.h>
#include <stdnoreturn.h>

#define _GNU_SOURCE
#ifndef __USE_MISC
#define __USE_MISC
#endif

#include <stdio.h>

const char *get_function_type_string(Type_Ptr_list arguments, Type *return_type) {
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

[[noreturn]]
void use_of_undeclared(const char *kind, const char *identifier, Span span) {
  char *span_string = lexer_span_to_string(span);
  fprintf(stderr, "use of undeclared %s '%s' at: %s", kind, identifier, span_string);
  exit(1);
}

Binding *get_binding(const char *identifier, Context *context);

Binding_Ptr_list typer_convert_parameters(Context *context, Parameter_list parameters, Span span,
                                          Type_Ptr_list *argument_types) {
  Binding_Ptr_list bindings = {0};
  LIST_FOREACH(parameters, param) {
    Thir_Ptr thir_param = thir_alloc(context, THIR_VARIABLE, span);
    Type *param_type = get_type_from_ast_type(param.type, context);

    if (!param_type) {
      fprintf(stderr, "use of undeclared type for parameter '%s' at: %s\n",
              param.nameless ? "<nameless parameter>" : param.name,
              lexer_span_to_string(span));
      exit(1);
    }

    Binding binding = {.thir = thir_param,
                       .ast = nullptr,
                       .name = param.nameless ? "<nameless parameter>" : param.name,
                       .type = param_type};

    Binding_Ptr ptr = bind_variable(context, binding);
    thir_param->binding = ptr;
    thir_param->type = param_type;

    LIST_PTR_PUSH(argument_types, param_type);
    LIST_PUSH(bindings, ptr);
  }

  return bindings;
}

Binding *get_binding(const char *identifier, Context *context) {
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

    switch (thir->tag) {
    case THIR_VARIABLE:
      if (strcmp(thir->binding->name, identifier) == 0) {
        return thir->binding;
      }
      break;
    case THIR_FUNCTION:
      if (strcmp(thir->function.name, identifier) == 0) {
        return thir->binding;
      }
      break;
    case THIR_EXTERN:
      if (strcmp(thir->extern_function.name, identifier) == 0) {
        return thir->binding;
      }
      break;
      break;
    default:
      break;
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

  return nullptr;
}

[[noreturn]]
void report_error(Ast *error) {
  if (error && error->tag == AST_ERROR) {
    const char *source_range = lexer_span_to_string(error->span);
    fprintf(stderr, "Error at span %s: '%s'\n", source_range, error->error.message ? error->error.message : "Unknown error");
  }
  exit(1);
}

Thir *type_program(Ast *ast, Context *context) {
  Thir *program = thir_alloc(context, THIR_PROGRAM, ast->span);
  Thir_Ptr_list statements = {0};

  LIST_FOREACH(ast->program, statement) {
    switch (statement->tag) {
    case AST_ERROR:
      report_error(statement);
      break;
    case AST_STRUCT: {
      type_struct(statement, context);
    } break;
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
  $extern->extern_function.parameters =
      typer_convert_parameters(context, ast->extern_function.parameters, ast->span, &argument_types);

  Type *return_type = get_type_from_ast_type(ast->extern_function.return_type, context);

  if (!return_type) {
    fprintf(stderr, "use of undeclared type as return type: '%s' at %s\n", ast->extern_function.return_type->type.path,
            lexer_span_to_string(ast->span));
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
  thir_fn->parameters = typer_convert_parameters(context, ast_fn.parameters, ast->span, &argument_types);

  Type *return_type = get_type_from_ast_type(ast_fn.return_type, context);

  if (!return_type) {
    fprintf(stderr, "use of undeclared type as return type: '%s' at %s\n", ast_fn.return_type->type.path,
            lexer_span_to_string(ast->span));
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
  case AST_LITERAL_INTEGER:
    literal->literal.value = ast->literal.value;
    literal->type = context->integer_type;
    break;
  case AST_LITERAL_STRING:
    literal->literal.value = ast->literal.value;
    literal->type = context->string_type;
    break;
  case AST_LITERAL_BOOL:
    literal->literal.value = ast->literal.value;
    literal->type = context->bool_type;
    break;
  }

  return literal;
}

Thir *type_identifier(Ast *ast, Context *context) {
  Binding *binding = get_binding(ast->identifier, context);

  if (!binding) {
    use_of_undeclared("identifier", ast->identifier, ast->span);
  }

  return binding->thir;
}

Thir *type_if(Ast *ast, Context *context) {
  Thir *condition = type_expression(ast->$if.condition, context);
  Thir *then_block = type_block(ast->$if.then_block, context);
  Thir *else_block = nullptr;
  if (ast->$if.else_block) {
    if (ast->$if.else_block->tag == AST_IF) {
      else_block = type_if(ast->$if.else_block, context);
    } else {
      else_block = type_block(ast->$if.else_block, context);
    }
  }
  Thir *thir = thir_alloc(context, THIR_IF, ast->span);
  thir->$if.condition = condition;
  thir->$if.else_block = else_block;
  thir->$if.then_block = then_block;
  return thir;
}

Thir *type_while(Ast *ast, Context *context) {
  Thir *condition = nullptr;
  if (ast->$while.condition) {
    condition = type_expression(ast->$while.condition, context);
  } else {
    static Thir *true_literal;
    if (!true_literal) {
      true_literal = thir_alloc(context, THIR_LITERAL, (Span){});
      true_literal->literal.value = "true";
      true_literal->type = context->bool_type;
    }

    condition = true_literal;
  }

  Thir *block = type_block(ast->$while.block, context);
  Thir *loop = thir_alloc(context, THIR_LOOP, ast->span);
  loop->loop.condition = condition;
  loop->loop.increment = nullptr;
  loop->loop.initializer = nullptr;
  loop->loop.block = block;
  return loop;
}

Thir *type_block(Ast *ast, Context *context) {
  Thir *block = thir_alloc(context, THIR_BLOCK, ast->span);

  Thir_Ptr_list statements = {0};
  LIST_FOREACH(ast->block, statement) {
    switch (statement->tag) {
    case AST_ERROR:
      report_error(statement);
    case AST_WHILE: 
      LIST_PUSH(statements, type_while(statement, context));
      break;
    case AST_IF:
      LIST_PUSH(statements, type_if(statement, context));
      break;
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
      fprintf(stderr, "unexpected statement type in block: %d at: %s\n", statement->tag, lexer_span_to_string(ast->span));
      exit(1);
    }
  }
  block->block = statements;

  return block;
}

Thir *type_expression(Ast *ast, Context *context) {
  switch (ast->tag) {
  case AST_MEMBER_ACCESS: {
    Thir *base = type_expression(ast->member_access.base, context);

    Thir *thir = thir_alloc(context, THIR_MEMBER_ACCESS, ast->span);
    thir->member_access.base = base;
    Type *base_type = base->type;

    if (base_type->tag != TYPE_STRUCT) {
      fprintf(stderr, "error: member access only allowed for structs at: %s\n", lexer_span_to_string(ast->span));
      exit(1);
    }

    Struct_Type *struct_type = (Struct_Type *)base_type;
    unsigned index = -1;
    LIST_FOREACH(struct_type->members, member) {
      if (strcmp(member.name, ast->member_access.member) == 0) {
        thir->type = member.type;
        index = __i;
        break;
      }
    }

    if (index < 0) {
      fprintf(stderr, "error: member '%s' not found in struct at: %s\n", ast->member_access.member,
              lexer_span_to_string(ast->span));
      exit(1);
    }

    thir->member_access.index = index;

    return thir;
  } break;
  case AST_AGGREGATE_INITIALIZER:
    return type_aggregate_initializer(ast, context);
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
    fprintf(stderr, "unexpected expression node type: %d, at %s\n", ast->tag, lexer_span_to_string(ast->span));
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

  if (ast->binary.op == OPERATOR_NONE) {
    fprintf(stderr, "invalid binary operator: OPERATOR_NONE at %s\n", lexer_span_to_string(ast->span));
    exit(1);
  }

  switch (ast->binary.op) {
  case OPERATOR_LOGICAL_OR:
  case OPERATOR_LOGICAL_AND:
  case OPERATOR_EQUALS:
  case OPERATOR_NOT_EQUALS:
  case OPERATOR_LESS:
  case OPERATOR_GREATER:
  case OPERATOR_LESS_EQUAL:
    binary->type = context->bool_type;
    break;

  case OPERATOR_DEREFERENCE:
  case OPERATOR_ADDRESS_OF:
    TODO("Pointers not implemented, therefore & and * unarys are not "
         "implemented");
  case OPERATOR_INDEX:
    Type *array_type = left->type;
    if (!type_is_array(array_type)) {
      fprintf(stderr, "unable to use index on non-array types currently\n");
      exit(1);
    }
    Type *index_type = right->type;
    if (index_type->tag != TYPE_INT) {
      fprintf(stderr, "cannot index into arrays with non-integer index\n");
      exit(1);
    }

    binary->type = array_type->pointee;
  case OPERATOR_XOR:
  case OPERATOR_PLUS_ASSIGN:
  case OPERATOR_MINUS_ASSIGN:
  case OPERATOR_STAR_ASSIGN:
  case OPERATOR_SLASH_ASSIGN:
  case OPERATOR_BIT_OR_ASSIGN:
  case OPERATOR_BIT_AND_ASSIGN:
  case OPERATOR_SHIFT_LEFT_ASSIGN:
  case OPERATOR_SHIFT_RIGHT_ASSIGN:
  case OPERATOR_NEGATE:
  case OPERATOR_ASSIGN:
  case OPERATOR_BIT_AND:
  case OPERATOR_BIT_OR:
  case OPERATOR_GREATER_EQUAL:
  case OPERATOR_SHIFT_LEFT:
  case OPERATOR_SHIFT_RIGHT:
  case OPERATOR_ADD:
  case OPERATOR_SUB:
  case OPERATOR_MUL:
  case OPERATOR_DIV:
  case OPERATOR_LOGICAL_NOT:
  case OPERATOR_BIT_NOT:
  case OPERATOR_MODULO:
  case OPERATOR_NONE: // shouldn't be possible
    binary->type = left->type;
    break;
  }

  binary->binary.op = ast->binary.op;
  binary->binary.left = left;
  binary->binary.right = right;

  return binary;
}

Thir *type_return(Ast *ast, Context *context) {
  Thir *ret = thir_alloc(context, THIR_RETURN, ast->span);
  if (ast->return_value) {
    ret->return_value = type_expression(ast->return_value, context);
    if (context->typer_expected_type && ret->return_value->type != context->typer_expected_type) {
      fprintf(stderr, "invalid return type at: %s. \"%s\" does not match expected \"%s\"\n", lexer_span_to_string(ast->span),
              ret->return_value->type->name, context->typer_expected_type->name);
      exit(1);
    }
  } else if (context->typer_expected_type) {
    fprintf(stderr,
            "invalid return type at: %s. this function must return a "
            "value.expected: \"%s\"\n",
            lexer_span_to_string(ast->span), context->typer_expected_type->name);
    exit(1);
  }
  return ret;
}

Thir *type_call(Ast *ast, Context *context) {
  Binding *callee = get_binding(ast->call.callee, context);

  if (!callee) {
    use_of_undeclared("function", ast->call.callee, ast->span);
  }

  Thir_Ptr_list arguments = {0};
  Function_Type *callee_type = (Function_Type *)callee->type;

  LIST_FOREACH(ast->call.arguments, ast_arg) {
    Type *old_expected = context->typer_expected_type;
    context->typer_expected_type = callee_type->parameters.data[__i];
    Thir *arg = type_expression(ast_arg, context);
    context->typer_expected_type = old_expected;
    LIST_PUSH(arguments, arg);
  }

  Thir *function = callee->thir;

  unsigned n_params = function->function.parameters.length;
  if (n_params != arguments.length) {
    fprintf(stderr, "invalid call at %s: too %s arguments. expected %d, but got %d.\n", lexer_span_to_string(ast->span),
            n_params > arguments.length ? "few" : "many", n_params, arguments.length);
    exit(1);
  }

  Thir *call = thir_alloc(context, THIR_CALL, ast->span);
  call->call.arguments = arguments;
  call->call.callee = callee;

  call->type = callee_type->returns; // Set our type to the return type.

  return call;
}

Type *get_type_from_ast_type(Ast *ast, Context *context) {
  Type *base_type = nullptr;

  LIST_FOREACH(context->type_table, type) {
    if (!type->name) {
      continue;
    }
    if (strcmp(type->name, ast->type.path) != 0) {
      continue;
    }

    if (type->extensions.length == 0) {
      base_type = type;
    }

    unsigned length = type->extensions.length;
    if (length != ast->type.extensions.length) {
      continue;
    }

    for (unsigned i = 0; i < length; ++i) {
      Type_Extension ast_extension = ast->type.extensions.data[i];
      Type_Extension extension = type->extensions.data[i];
      if (ast_extension != extension) {
        goto L_END_OF_EXTENSION_MATCHING_LOOP;
      }
    }

    // names & extensions match, return type.
    return type;

  L_END_OF_EXTENSION_MATCHING_LOOP:
  }

  // found the base type, but not the extended type.
  if (base_type) {
    Type *extended_type = type_alloc(context);
    extended_type->extensions = ast->type.extensions;
    extended_type->pointee = base_type;
    extended_type->name = base_type->name;
    extended_type->tag = base_type->tag;
    return extended_type;
  }

  // found nothing.
  return nullptr;
}

Thir *type_variable(Ast *ast, Context *context) {
  Thir *var = thir_alloc(context, THIR_VARIABLE, ast->span);

  Thir *initializer = nullptr;

  if (!ast->variable.type) {
    fprintf(stderr, "un-typed variables not allowed: %d, at: %s\n", ast->variable.initializer->tag,
            lexer_span_to_string(ast->variable.initializer->span));
    exit(1);
  }

  Type *type = get_type_from_ast_type(ast->variable.type, context);

  if (ast->variable.initializer) {
    Type *old_expected = context->typer_expected_type;
    context->typer_expected_type = type;
    initializer = type_expression(ast->variable.initializer, context);
    context->typer_expected_type = old_expected;
  } else {
    fprintf(stderr, "uninitialized variables not allowed: %d, at: %s\n", ast->variable.initializer->tag,
            lexer_span_to_string(ast->variable.initializer->span));
    exit(1);
  }

  if (initializer->type != type) {
    fprintf(stderr, "invalid type in declaration at: %s, expected %s, but got %s.\n", lexer_span_to_string(ast->span),
            type->name, initializer->type->name);
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

Thir *type_aggregate_initializer(Ast *ast, Context *context) {
  Type *type = context->typer_expected_type;

  // if you just provide one value, such as {0}, and it's not a struct nor array, just do
  if (type->tag != TYPE_STRUCT && !type_is_array(type) && ast->aggregate_initializer.keys.length == 0 &&
      ast->aggregate_initializer.values.length == 1) {
    return type_expression(ast->aggregate_initializer.values.data[0], context);
  }

  Type_Ptr_list value_types = {0};
  Thir_Ptr_list values = {0};
  LIST_FOREACH(ast->aggregate_initializer.values, value) {
    Thir *v;
    LIST_PUSH(values, v = type_expression(value, context));
    LIST_PUSH(value_types, v->type);
  }

  if (type->tag == TYPE_STRUCT) {
    if (ast->aggregate_initializer.keys.length == 0) {
      Struct_Type *struct_type = (Struct_Type *)type;
      string_list keys = {0};
      LIST_FOREACH(struct_type->members, member) {
        // off-by-one: when __i == value_types.length, we've run out of values
        if (value_types.length <= __i) {
          fprintf(stderr,
                  "too few values provided for aggregate initializer at: %s, "
                  "expected: %d, got: %d\n",
                  lexer_span_to_string(ast->span), struct_type->members.length, value_types.length);
          exit(1);
        }
        if (value_types.data[__i] != member.type) {
          fprintf(stderr,
                  "invalid type in aggregate initializer at: %s, expected %s, "
                  "got %s. (aggregate initializers must match the layout of "
                  "the struct when used without keys)\n",
                  lexer_span_to_string(ast->span), member.type->name, value_types.data[__i]->name);
          exit(1);
        }
        LIST_PUSH(keys, member.name);
      }
      Thir *thir = thir_alloc(context, THIR_AGGREGATE_INITIALIZER, ast->span);
      thir->aggregate_initializer.values = values;
      thir->aggregate_initializer.keys = keys;
      thir->type = type;
      LIST_FREE(value_types);
      return thir;
    } else {
      // Validate keys and types, then reorder (keyed) values to struct layout.
      string_list keys = ast->aggregate_initializer.keys;
      Struct_Type *struct_type = (Struct_Type *)type;

      // 1) Validate provided keys exist and have matching types
      for (unsigned int i = 0; i < keys.length; ++i) {
        const char *key = keys.data[i];
        int found = 0;
        for (unsigned int j = 0; j < struct_type->members.length; ++j) {
          if (strcmp(struct_type->members.data[j].name, key) == 0) {
            if (value_types.data[i] != struct_type->members.data[j].type) {
              fprintf(stderr,
                      "invalid type for key '%s' in aggregate initializer at: "
                      "%s, expected %s, got %s.\n",
                      key, lexer_span_to_string(ast->span), struct_type->members.data[j].type->name,
                      value_types.data[i]->name);
              exit(1);
            }
            found = 1;
            break;
          }
        }
        if (!found) {
          fprintf(stderr, "unknown key '%s' in aggregate initializer at: %s\n", key, lexer_span_to_string(ast->span));
          exit(1);
        }
      }

      Thir_Ptr_list reordered_values = {0};
      string_list reordered_keys = {0};

      for (unsigned int m = 0; m < struct_type->members.length; ++m) {
        const char *member_name = struct_type->members.data[m].name;

        // find this member in the provided keys
        int provided_idx = -1;
        for (unsigned int i = 0; i < keys.length; ++i) {
          if (strcmp(keys.data[i], member_name) == 0) {
            provided_idx = (int)i;
            break;
          }
        }

        // If the user did not provide a value for this member, skip it.
        // (Alternative: error out if you require full coverage.)
        if (provided_idx < 0)
          continue;

        LIST_PUSH(reordered_values, values.data[provided_idx]);
        LIST_PUSH(reordered_keys, member_name);
      }

      Thir *thir = thir_alloc(context, THIR_AGGREGATE_INITIALIZER, ast->span);
      thir->aggregate_initializer.values = reordered_values;
      thir->aggregate_initializer.keys = reordered_keys;
      thir->type = type;

      LIST_FREE(value_types);
      return thir;
    }
  } else if (type_is_array(type)) {
    if (ast->aggregate_initializer.keys.length) {
      fprintf(stderr, "error: array initializers must use values only, not keys, at: %s\n", lexer_span_to_string(ast->span));
      exit(1);
    }
    Thir *thir = thir_alloc(context, THIR_ARRAY_INITIALIZER, ast->span);
    thir->array_initializer.values = values;
    thir->type = type;
    return thir;
  } else {
    fprintf(stderr,
            "currently unsupported type for multi-value aggregate initializers "
            "at %s, type \"%s\"\n",
            lexer_span_to_string(ast->span), type->name);
    exit(1);
  }
}

void type_struct(Ast *ast, Context *context) {

  Type *unused;
  (void)unused;
  if (try_find_type(context, ast->$struct.name, &unused)) {
    fprintf(stderr, "redefinition of struct '%s' at: %s\n", ast->$struct.name, lexer_span_to_string(ast->span));
    exit(1);
  }

  Struct_Type *type = struct_type_alloc(context, ast->$struct.name);
  LIST_FOREACH(ast->$struct.members, member) {
    Struct_Member struct_member;
    struct_member.name = member.name;
    struct_member.type = get_type_from_ast_type(member.type, context);
    LIST_PUSH(type->members, struct_member);
  }
}
