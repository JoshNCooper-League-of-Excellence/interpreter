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
#include <stdio.h>
#include <stdnoreturn.h>

Thir *type_program(Ast *ast, Context *c) {
  LIST_FOREACH(ast->program, stmt) {
    if (stmt->tag == AST_ERROR) {
      report_error(stmt);
    } else if (stmt->tag == AST_STRUCT) {
      Type *unused;
      if (!try_find_type(c, stmt->$struct.name, &unused)) {
        (void)struct_type_alloc(c, stmt->$struct.name);
      }
    }
  }

  LIST_FOREACH(ast->program, stmt) {
    switch (stmt->tag) {
    case AST_EXTERN: {
      Type_Ptr_list arg_types = collect_parameter_types(stmt->extern_function.parameters, stmt->span, c);
      Type *ret = get_type_from_ast_type(stmt->extern_function.return_type, c);
      Function_Type *fty;
      if (!try_find_function_type(c, arg_types, ret, &fty)) {
        fty = function_type_alloc(c);
        fty->base.name = get_function_type_string(arg_types, ret);
        fty->parameters = arg_types;
        fty->returns = ret;
      }
      Binding b = (Binding){0};
      b.ast = stmt;
      b.name = stmt->extern_function.name;
      b.type = (Type *)fty;
      bind_function(c, b, true);
    } break;
    case AST_FUNCTION: {
      Type_Ptr_list arg_types = collect_parameter_types(stmt->function.parameters, stmt->span, c);
      Type *ret = get_type_from_ast_type(stmt->function.return_type, c);
      Function_Type *fty;
      if (!try_find_function_type(c, arg_types, ret, &fty)) {
        fty = function_type_alloc(c);
        fty->base.name = get_function_type_string(arg_types, ret);
        fty->parameters = arg_types;
        fty->returns = ret;
      }
      Binding b = (Binding){0};
      b.ast = stmt;
      b.name = stmt->function.name;
      b.type = (Type *)fty;
      bind_function(c, b, false);
    } break;
    default:
      break;
    }
  }

  LIST_FOREACH(ast->program, stmt) {
    if (stmt->tag == AST_STRUCT) {
      Type *exists;
      if (try_find_type(c, stmt->$struct.name, &exists)) {
        Struct_Type *st = (Struct_Type *)exists;
        st->members.length = 0;
        LIST_FOREACH(stmt->$struct.members, m) {
          Struct_Member sm = {.name = m.name, .type = get_type_from_ast_type(m.type, c)};
          LIST_PUSH(st->members, sm);
        }
      }
    }
  }

  Thir *program = thir_alloc(c, THIR_PROGRAM, ast->span);
  Thir_Ptr_list statements = {0};

  LIST_FOREACH(ast->program, stmt) {
    if (stmt->tag == AST_EXTERN) {
      Thir *e = type_extern(stmt, c);
      LIST_PUSH(statements, e);
    }
  }

  LIST_FOREACH(ast->program, stmt) {
    if (stmt->tag == AST_FUNCTION) {
      Thir *f = type_function(stmt, c);
      LIST_PUSH(statements, f);
    }
  }

  program->program = statements;
  return program;
}

Thir *type_function(Ast *ast, Context *c) {
  Binding *existing = get_binding(ast->function.name, c);
  if (existing && existing->thir)
    return existing->thir;

  Thir *thir = thir_alloc(c, THIR_FUNCTION, ast->span);
  thir->function.name = ast->function.name;

  Scope *old_scope = scope_enter_new_child_and_return_previous(c);

  Type_Ptr_list argument_types = {0};
  thir->function.parameters =
      convert_ast_parameters_to_thir_parameters(c, ast->function.parameters, ast->span, &argument_types);

  Type *return_type = get_type_from_ast_type(ast->function.return_type, c);
  if (!return_type) {
    fprintf(stderr, "use of undeclared type as return type: '%s' at %s\n", ast->function.return_type->type.path,
            lexer_span_to_string(ast->span));
    exit(1);
  }

  // Prefer the function type from the existing binding if available
  Function_Type *fty = NULL;
  if (existing && existing->type) {
    fty = (Function_Type *)existing->type;
  } else {
    if (!try_find_function_type(c, argument_types, return_type, &fty)) {
      fty = function_type_alloc(c);
      fty->base.name = get_function_type_string(argument_types, return_type);
      fty->parameters = argument_types;
      fty->returns = return_type;
    }
  }

  thir->function.return_type = return_type;
  c->typer_expected_type = thir->function.return_type;
  thir->function.block = type_block(ast->function.block, c);
  c->typer_expected_type = nullptr;
  c->current_scope = old_scope;
  thir->type = (Type *)fty;

  // Fulfill binding instead of rebinding
  if (existing) {
    existing->thir = thir;
    existing->type = (Type *)fty;
    existing->ast = ast;
  } else {
    assert(false && "[THIR] function should have already been bound by now");
  }

  return thir;
}

Thir *type_extern(Ast *ast, Context *c) {
  Binding *existing = get_binding(ast->extern_function.name, c);
  if (existing && existing->thir) {
    return existing->thir;
  }

  Thir *thir = thir_alloc(c, THIR_EXTERN, ast->span);
  const char *name = ast->extern_function.name;
  thir->extern_function.name = name;
  Type_Ptr_list argument_types = collect_parameter_types(ast->extern_function.parameters, ast->span, c);
  thir->extern_function.parameters = (Binding_Ptr_list){0};

  Type *return_type = get_type_from_ast_type(ast->extern_function.return_type, c);
  if (!return_type) {
    fprintf(stderr, "use of undeclared type as return type: '%s' at %s\n", ast->extern_function.return_type->type.path,
            lexer_span_to_string(ast->span));
    exit(1);
  }

  thir->extern_function.return_type = return_type;

  Function_Type *fty = NULL;
  if (!try_find_function_type(c, argument_types, return_type, &fty)) {
    fty = function_type_alloc(c);
    fty->base.name = get_function_type_string(argument_types, return_type);
    fty->parameters = argument_types;
    fty->returns = return_type;
  }

  if (!fty) {
    fprintf(stderr, "failed to allocate function type for extern at: %s\n", lexer_span_to_string(ast->span));
    exit(1);
  }

  thir->type = (Type *)fty;
  
  if (existing && !existing->thir) {
    existing->thir = thir;
    thir->binding = existing;
  } else {
    assert(false && "[THIR] extern should have already been bound by now");
  }


  Extern_Function ffi_function = get_ffi_function_from_thir(thir);
  thir->extern_function.index = ffi_function.index;

  return thir;
}

Thir *type_literal(Ast *ast, Context *c) {
  Thir *literal = thir_alloc(c, THIR_LITERAL, ast->span);

  switch (ast->literal.tag) {
  case AST_LITERAL_INTEGER:
    literal->literal.value = ast->literal.value;
    literal->type = c->integer_type;
    break;
  case AST_LITERAL_STRING:
    literal->literal.value = ast->literal.value;
    literal->type = c->string_type;
    break;
  case AST_LITERAL_BOOL:
    literal->literal.value = ast->literal.value;
    literal->type = c->bool_type;
    break;
  }

  return literal;
}

Thir *type_identifier(Ast *ast, Context *c) {
  Binding *binding = get_binding(ast->identifier, c);

  if (!binding) {
    use_of_undeclared("identifier", ast->identifier, ast->span);
  }

  return binding->thir;
}

Thir *type_if(Ast *ast, Context *c) {
  Thir *condition = type_expression(ast->$if.condition, c);
  Thir *then_block = type_block(ast->$if.then_block, c);
  Thir *else_block = nullptr;
  if (ast->$if.else_block) {
    if (ast->$if.else_block->tag == AST_IF) {
      else_block = type_if(ast->$if.else_block, c);
    } else {
      else_block = type_block(ast->$if.else_block, c);
    }
  }
  Thir *thir = thir_alloc(c, THIR_IF, ast->span);
  thir->$if.condition = condition;
  thir->$if.else_block = else_block;
  thir->$if.then_block = then_block;
  return thir;
}

Thir *type_while(Ast *ast, Context *c) {
  Thir *condition = nullptr;
  if (ast->$while.condition) {
    condition = type_expression(ast->$while.condition, c);
  } else {
    static Thir *true_literal;
    if (!true_literal) {
      true_literal = thir_alloc(c, THIR_LITERAL, (Span){});
      true_literal->literal.value = "true";
      true_literal->type = c->bool_type;
    }

    condition = true_literal;
  }

  Thir *block = type_block(ast->$while.block, c);
  Thir *loop = thir_alloc(c, THIR_LOOP, ast->span);
  loop->loop.condition = condition;
  loop->loop.update = nullptr;
  loop->loop.init = nullptr;
  loop->loop.block = block;
  return loop;
}

Thir *type_for(Ast *ast, Context *c) {
  Thir *init = type_variable(ast->$for.init, c);
  Thir *condition = type_expression(ast->$for.condition, c);
  Thir *update = type_expression(ast->$for.update, c);
  Thir *block = type_block(ast->$for.block, c);
  Thir *loop = thir_alloc(c, THIR_LOOP, ast->span);
  loop->loop.init = init;
  loop->loop.update = update;
  loop->loop.condition = condition;
  loop->loop.block = block;
  return loop;
}
Thir *type_block(Ast *ast, Context *c) {
  Thir *block = thir_alloc(c, THIR_BLOCK, ast->span);

  Scope *old_scope = scope_enter_new_child_and_return_previous(c);

  Thir_Ptr_list statements = {0};
  LIST_FOREACH(ast->block, statement) {
    switch (statement->tag) {
    case AST_ERROR:
      report_error(statement);
    case AST_FOR:
      LIST_PUSH(statements, type_for(statement, c));
      break;
    case AST_WHILE:
      LIST_PUSH(statements, type_while(statement, c));
      break;
    case AST_IF:
      LIST_PUSH(statements, type_if(statement, c));
      break;
    case AST_CALL:
      LIST_PUSH(statements, type_call(statement, c));
      break;
    case AST_UNARY:
      LIST_PUSH(statements, type_unary(statement, c));
      break;
    case AST_BINARY:
      LIST_PUSH(statements, type_binary(statement, c));
      break;
    case AST_RETURN:
      LIST_PUSH(statements, type_return(statement, c));
      break;
    case AST_VARIABLE:
      LIST_PUSH(statements, type_variable(statement, c));
      break;
    default:
      fprintf(stderr, "unexpected statement type in block: %d at: %s\n", statement->tag, lexer_span_to_string(ast->span));
      exit(1);
    }
  }
  block->block = statements;
  c->current_scope = old_scope;
  return block;
}

Thir *type_expression(Ast *ast, Context *c) {
  switch (ast->tag) {
  case AST_MEMBER_ACCESS: {
    Thir *base = type_expression(ast->member_access.base, c);

    Thir *thir = thir_alloc(c, THIR_MEMBER_ACCESS, ast->span);
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
    return type_aggregate_initializer(ast, c);
  case AST_CALL:
    return type_call(ast, c);
  case AST_LITERAL:
    return type_literal(ast, c);
  case AST_IDENTIFIER:
    return type_identifier(ast, c);
  case AST_UNARY:
    return type_unary(ast, c);
  case AST_BINARY:
    return type_binary(ast, c);
  default:
    fprintf(stderr, "unexpected expression node type: %d, at %s\n", ast->tag, lexer_span_to_string(ast->span));
    exit(1);
  }
}

Thir *type_unary(Ast *ast, Context *c) {
  Thir *unary = thir_alloc(c, THIR_UNARY, ast->span);
  Thir *operand = type_expression(ast->unary.operand, c);

  unary->unary.op = ast->unary.op;
  unary->unary.operand = operand;
  unary->type = operand->type;
  return unary;
}

Thir *type_binary(Ast *ast, Context *c) {
  Thir *binary = thir_alloc(c, THIR_BINARY, ast->span);

  Thir *left = type_expression(ast->binary.left, c);
  Thir *right = type_expression(ast->binary.right, c);

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
    binary->type = c->bool_type;
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

Thir *type_return(Ast *ast, Context *c) {
  Thir *ret = thir_alloc(c, THIR_RETURN, ast->span);
  if (ast->return_value) {
    ret->return_value = type_expression(ast->return_value, c);
    if (c->typer_expected_type && ret->return_value->type != c->typer_expected_type) {
      fprintf(stderr, "invalid return type at: %s. \"%s\" does not match expected \"%s\"\n", lexer_span_to_string(ast->span),
              ret->return_value->type->name, c->typer_expected_type->name);
      exit(1);
    }
  } else if (c->typer_expected_type) {
    fprintf(stderr,
            "invalid return type at: %s. this function must return a "
            "value.expected: \"%s\"\n",
            lexer_span_to_string(ast->span), c->typer_expected_type->name);
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

  unsigned n_params = callee_type->parameters.length;
  if (n_params != arguments.length) {
    fprintf(stderr, "invalid call at %s: too %s arguments. expected %u, but got %u.\n", lexer_span_to_string(ast->span),
            n_params > arguments.length ? "few" : "many", n_params, arguments.length);
    exit(1);
  }

  Thir *call = thir_alloc(context, THIR_CALL, ast->span);
  call->call.arguments = arguments;
  call->call.callee = callee;
  call->type = callee_type->returns;

  return call;
}

Type *get_type_from_ast_type(Ast *ast, Context *c) {
  Type *base_type = nullptr;

  LIST_FOREACH(c->type_table, type) {
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
    Type *extended_type = type_alloc(c);
    extended_type->extensions = ast->type.extensions;
    extended_type->pointee = base_type;
    extended_type->name = base_type->name;
    extended_type->tag = base_type->tag;
    return extended_type;
  }

  // found nothing.
  return nullptr;
}

Thir *type_variable(Ast *ast, Context *c) {
  Thir *var = thir_alloc(c, THIR_VARIABLE, ast->span);

  Thir *initializer = nullptr;

  if (!ast->variable.type) {
    fprintf(stderr, "un-typed variables not allowed: %d, at: %s\n", ast->variable.initializer->tag,
            lexer_span_to_string(ast->variable.initializer->span));
    exit(1);
  }

  Type *type = get_type_from_ast_type(ast->variable.type, c);

  if (ast->variable.initializer) {
    Type *old_expected = c->typer_expected_type;
    c->typer_expected_type = type;
    initializer = type_expression(ast->variable.initializer, c);
    c->typer_expected_type = old_expected;
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
  bind_variable(c, binding);

  var->type = initializer->type;
  var->variable_initializer = initializer;

  return var;
}

Thir *type_aggregate_initializer(Ast *ast, Context *c) {
  Type *type = c->typer_expected_type;

  // if you just provide one value, such as {0}, and it's not a struct nor array, just do
  if (type->tag != TYPE_STRUCT && !type_is_array(type) && ast->aggregate_initializer.keys.length == 0 &&
      ast->aggregate_initializer.values.length == 1) {
    return type_expression(ast->aggregate_initializer.values.data[0], c);
  }

  Type_Ptr_list value_types = {0};
  Thir_Ptr_list values = {0};
  LIST_FOREACH(ast->aggregate_initializer.values, value) {
    Thir *v;
    LIST_PUSH(values, v = type_expression(value, c));
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
      Thir *thir = thir_alloc(c, THIR_AGGREGATE_INITIALIZER, ast->span);
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

      Thir *thir = thir_alloc(c, THIR_AGGREGATE_INITIALIZER, ast->span);
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
    Thir *thir = thir_alloc(c, THIR_ARRAY_INITIALIZER, ast->span);
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

Binding_Ptr_list convert_ast_parameters_to_thir_parameters(Context *context, Parameter_list parameters, Span span,
                                                           Type_Ptr_list *argument_types) {
  Binding_Ptr_list bindings = {0};
  LIST_FOREACH(parameters, param) {
    Thir_Ptr thir_param = thir_alloc(context, THIR_VARIABLE, span);
    Type *param_type = get_type_from_ast_type(param.type, context);

    if (!param_type) {
      fprintf(stderr, "use of undeclared type for parameter '%s' at: %s\n",
              param.nameless ? "<nameless parameter>" : param.name, lexer_span_to_string(span));
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

Type_Ptr_list collect_parameter_types(Parameter_list params, Span span, Context *ctx) {
  Type_Ptr_list types = {0};
  LIST_FOREACH(params, p) {
    Type *t = get_type_from_ast_type(p.type, ctx);
    if (!t) {
      fprintf(stderr, "use of undeclared type for parameter '%s' at: %s\n", p.nameless ? "<nameless parameter>" : p.name,
              lexer_span_to_string(span));
      exit(1);
    }
    LIST_PTR_PUSH(&types, t);
  }
  return types;
}

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