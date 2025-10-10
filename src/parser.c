#include "parser.h"
#include "ast.h"
#include "binding.h"
#include "lexer.h"
#include "list.h"
#include <limits.h>

void print_ast_function_header(Ast *function) {
  printf("%s :: (", function->function.name);
  LIST_FOREACH(function->function.parameters, param) {
    printf("%s %s", param.name, param.type->type.path);
    if (__i != function->function.parameters.length - 1) {
      printf(", ");
    }
  }
  printf(") %s\n", function->function.return_type);
}

#define EXPECT($expected)                                                      \
  ({                                                                           \
    Token tok = lexer_peek(lexer);                                             \
    if (tok.type != $expected) {                                               \
      char *error_msg;                                                         \
      asprintf(                                                                \
          &error_msg, "unexpected token at %s:%zu:%zu, expected %s, got %s\n", \
          lexer->filename, tok.span.line, tok.span.col,                        \
          token_type_to_string($expected), token_type_to_string(tok.type));    \
      return parser_error(context, lexer_span(lexer), error_msg);              \
    }                                                                          \
    lexer_eat(lexer);                                                          \
  })

#define OK($ast)                                                               \
  ({                                                                           \
    Ast *the_ast = $ast;                                                       \
    if (!the_ast || the_ast->tag == AST_ERROR) {                               \
      return the_ast;                                                          \
    }                                                                          \
    the_ast;                                                                   \
  })

#define BEGIN_SPAN(tok)                                                        \
  Span span;                                                                   \
  span = tok.span;

#define END_SPAN() span.length = lexer_peek(lexer).span.start - span.start;

Ast *parser_error(Context *context, Span span, const char *message) {
  fprintf(stderr, "%s:%zu:%zu: %s\n", CURRENTLY_COMPILING_FILE_NAME, span.line,
          span.col, message);
  Ast *ast = ast_alloc(context, AST_ERROR, span);
  ast->error.message = message;
  return ast;
}

Ast *parse_file(const char *filename, Context *context) {
  Lexer lexer;
  lexer_init(&lexer, filename);
  return parse_program(&lexer, context);
}

Ast *parse_program(Lexer *lexer, Context *context) {
  Ast_Ptr_list statements = {0};
  while (true) {
    Token_Type peeked = lexer_next(lexer);
    if (peeked == TOKEN_EOF) {
      break;
    }

    switch (lexer_next(lexer)) {
    case TOKEN_STRUCT: {
      LIST_PUSH(statements, OK(parse_struct(lexer, context)));
    } break;
    case TOKEN_IDENTIFIER: {
      LIST_PUSH(statements, OK(parse_identifier(lexer, context)));
      break;
    }
    case TOKEN_EXTERN: {
      LIST_PUSH(statements, OK(parse_extern(lexer, context)));
      EXPECT(TOKEN_SEMI);
      break;
    }
    default:
      char *buf;
      asprintf(&buf, "Unexpected token %s at top level.",
               token_type_to_string(peeked));
      return parser_error(context, lexer_span(lexer), buf);
      break;
    }
  }
  Ast *program = ast_alloc(context, AST_PROGRAM, lexer_span(lexer));
  program->program = statements;
  return program;
}

Ast *parse_block(Lexer *lexer, Context *context) {
  BEGIN_SPAN(EXPECT(TOKEN_LCURLY))
  Ast_Ptr_list statements = {0};

  while (true) {
    Token peeked = lexer_peek(lexer);
    if (peeked.type == TOKEN_RCURLY) {
      break;
    }

    bool expect_semi = true;
    switch (peeked.type) {
    case TOKEN_IF:
      expect_semi = false;
      LIST_PUSH(statements, OK(parse_if(lexer, context)));
      break;
    case TOKEN_SEMI:
      lexer_eat(lexer);
      break;
    case TOKEN_IDENTIFIER:
      LIST_PUSH(statements, OK(parse_identifier(lexer, context)));
      break;
    case TOKEN_RETURN:
      lexer_eat(lexer);
      if (lexer_next_is(lexer, TOKEN_SEMI)) {
        END_SPAN()
        Ast *return_expr = ast_alloc(context, AST_RETURN, span);
        return_expr->return_value = nullptr;
        LIST_PUSH(statements, return_expr);
        break;
      }
      Ast *expression = OK(parse_expression(lexer, context));
      END_SPAN();
      Ast *return_expr = ast_alloc(context, AST_RETURN, span);
      return_expr->return_value = expression;
      LIST_PUSH(statements, return_expr);
      break;
    case TOKEN_EOF:
      return parser_error(context, span,
                          "Unexpected end of input while parsing block");
    default:
      // easily propagate formatted error.
      // -2 doesn't exist.
      EXPECT(-2);
      break;
    }
    if (expect_semi) {
      EXPECT(TOKEN_SEMI);
    }
  }

  EXPECT(TOKEN_RCURLY);
  END_SPAN();

  Ast *block = ast_alloc(context, AST_BLOCK, span);
  block->block = statements;
  return block;
}

Ast *parse_call(const char *callee, Lexer *lexer, Context *context) {
  BEGIN_SPAN(EXPECT(TOKEN_LPAREN));
  Ast_Ptr_list arguments = {0};
  while (true) {
    if (lexer_next(lexer) == TOKEN_RPAREN) {
      break;
    }
    LIST_PUSH(arguments, OK(parse_expression(lexer, context)));
    if (!lexer_next_is(lexer, TOKEN_RPAREN)) {
      EXPECT(TOKEN_COMMA);
    }
  }
  EXPECT(TOKEN_RPAREN);
  END_SPAN()
  Ast *call = ast_alloc(context, AST_CALL, span);
  call->call.callee = callee;
  call->call.arguments = arguments;
  return call;
}

Ast *parse_postfix(Lexer *lexer, Context *context) {
  Ast *operand = parse_primary(lexer, context);
  while (lexer_next(lexer) == TOKEN_DOT) {
    BEGIN_SPAN(lexer_eat(lexer));
    Token member = EXPECT(TOKEN_IDENTIFIER);
    END_SPAN()
    Ast *member_access = ast_alloc(context, AST_MEMBER_ACCESS, span);
    member_access->member_access.base = operand;
    member_access->member_access.member = member.value;
    operand = member_access;
  }
  return operand;
}

Ast *parse_primary(Lexer *lexer, Context *context) {
  Token peeked = lexer_peek(lexer);
  BEGIN_SPAN(peeked);
  switch (peeked.type) {
  case TOKEN_LPAREN: {
    lexer_eat(lexer);
    Ast *expression = OK(parse_expression(lexer, context));
    EXPECT(TOKEN_RPAREN);
    return expression;
  }
  case TOKEN_TRUE:
  case TOKEN_FALSE: {
    lexer_eat(lexer);
    END_SPAN();
    Ast *ast = ast_alloc(context, AST_LITERAL, span);
    ast->literal.tag = AST_LITERAL_BOOL;
    ast->literal.value = peeked.type == TOKEN_TRUE ? "true" : "false";
    return ast;
  }
  case TOKEN_LCURLY: {
    return parse_aggregate_initializer(lexer, context);
  }
  case TOKEN_IDENTIFIER: {
    lexer_eat(lexer);
    if (lexer_next_is(lexer, TOKEN_LPAREN)) {
      return parse_call(peeked.value, lexer, context);
    }
    END_SPAN()
    Ast *ident = ast_alloc(context, AST_IDENTIFIER, span);
    ident->identifier = peeked.value;
    return ident;
  }
  case TOKEN_INTEGER: {
    lexer_eat(lexer);
    END_SPAN()
    Ast *integer = ast_alloc(context, AST_LITERAL, span);
    integer->literal.tag = AST_LITERAL_INTEGER;
    integer->literal.value = peeked.value;
    return integer;
  }
  case TOKEN_STRING: {
    lexer_eat(lexer);
    END_SPAN()
    Ast *integer = ast_alloc(context, AST_LITERAL, span);
    integer->literal.tag = AST_LITERAL_STRING;
    integer->literal.value = peeked.value;
    return integer;
  }
  default:
    lexer_eat(lexer);
    char *buf;
    asprintf(&buf, "unexpected token when parsing literal: %s",
             token_type_to_string(peeked.type));
    return parser_error(context, span, buf);
  }
}

Ast *parse_unary(Lexer *lexer, Context *context) {
  BEGIN_SPAN(lexer_peek(lexer));
  Ast *error = NULL;
  Operator op = parse_operator(lexer, context, EXPR_UNARY, &error);
  bool is_valid_operator = (op != OPERATOR_NONE);

  if (error) {
    return error;
  }

  if (is_valid_operator) {
    Ast *operand = OK(parse_unary(lexer, context));
    Ast *unary = ast_alloc(context, AST_UNARY, span);
    unary->unary.op = op;
    unary->unary.operand = operand;
    return unary;
  }
  return parse_postfix(lexer, context);
}

Ast *parse_binary(Lexer *lexer, Context *context, Precedence precedence) {
  BEGIN_SPAN(lexer_peek(lexer));
  Ast *left = OK(parse_unary(lexer, context));
  while (true) {
    Ast *error = nullptr;
    Operator operator = parse_operator(lexer, context, EXPR_BINARY, &error);

    if (error) {
      return error;
    }

    bool is_valid_operator = false;
    Precedence op_prec = get_precedence(operator, &is_valid_operator);

    if (op_prec < precedence || !is_valid_operator) {
      break;
    }

    Ast *right = OK(parse_binary(lexer, context, op_prec + 1));

    END_SPAN();
    Ast *bin = ast_alloc(context, AST_BINARY, span);
    bin->binary.left = left;
    bin->binary.right = right;
    bin->binary.op = operator;
    left = bin;
  }
  return left;
}

Ast *parse_if(Lexer *lexer, Context *context) {
  BEGIN_SPAN(EXPECT(TOKEN_IF));
  Ast *condition = OK(parse_expression(lexer, context));
  Ast *then_block = OK(parse_block(lexer, context));

  Ast *else_block = nullptr;
  if (lexer_next(lexer) == TOKEN_ELSE) {
    lexer_eat(lexer);
    if (lexer_next(lexer) == TOKEN_IF) {
      else_block = OK(parse_if(lexer, context));
    } else {
      else_block = OK(parse_block(lexer, context));
    }
  }

  END_SPAN()
  Ast *ast = ast_alloc(context, AST_IF, span);
  ast->$if.condition = condition;
  ast->$if.else_block = else_block;
  ast->$if.then_block = then_block;
  return ast;
}

Ast *parse_identifier(Lexer *lexer, Context *context) {
  // The peeked is ALWAYS an identifier here, so the other lookaheads are beyond
  // that.
  Token two_ahead = lexer_lookahead(lexer, 1);

  // x int = ... variable declaration
  if (two_ahead.type == TOKEN_IDENTIFIER) {
    return parse_variable(lexer, context);
  }

  // main :: ... functiond declaration
  if (two_ahead.type == TOKEN_COLON) {
    return parse_function(lexer, context);
  }

  return parse_expression(lexer, context);
}

Ast *parse_type(Lexer *lexer, Context *context) {
  Token name = EXPECT(TOKEN_IDENTIFIER);
  Ast *ast = ast_alloc(context, AST_TYPE, name.span);
  ast->type.path = name.value;
  return ast;
}

Ast *parse_expression(Lexer *lexer, Context *context) {
  return parse_binary(lexer, context, PREC_NONE);
}

bool parse_function_header(Lexer *lexer, Context *context, const char **name,
                           Parameter_list *parameters, const char **return_type,
                           Span *span) {
  Token identifier = EXPECT(TOKEN_IDENTIFIER);
  *name = identifier.value;
  *span = identifier.span;
  EXPECT(TOKEN_COLON);
  EXPECT(TOKEN_COLON);
  EXPECT(TOKEN_LPAREN);

  *parameters = (Parameter_list){0};
  while (true) {
    Token peeked = lexer_peek(lexer);
    if (peeked.type == TOKEN_RPAREN) {
      break;
    }

    Token one_ahead = lexer_lookahead(lexer, 1);
    // Parse a type, no name for parameter.
    if (lexer_next(lexer) != TOKEN_IDENTIFIER ||
        one_ahead.type == TOKEN_COMMA || one_ahead.type == TOKEN_RPAREN) {
      Parameter parameter = {.type = OK(parse_type(lexer, context)),
                             .nameless = true};
      LIST_PTR_PUSH(parameters, parameter);
    } else { // Parse $name $Type pair.
      Token param_name = EXPECT(TOKEN_IDENTIFIER);
      Parameter parameter = {.name = param_name.value,
                             .type = OK(parse_type(lexer, context)),
                             .nameless = false};
      LIST_PTR_PUSH(parameters, parameter);
    }

    if (lexer_next(lexer) != TOKEN_RPAREN) {
      EXPECT(TOKEN_COMMA);
    }
  }

  EXPECT(TOKEN_RPAREN);
  Token returns = EXPECT(TOKEN_IDENTIFIER);
  *return_type = returns.value;
  return true;
}

Ast *parse_function(Lexer *lexer, Context *context) {
  Span span;
  const char *name;
  Parameter_list parameters = {0};
  const char *return_type;

  if (!parse_function_header(lexer, context, &name, &parameters, &return_type,
                             &span)) {
    return NULL;
  }

  LIST_FOREACH(parameters, param) {
    if (param.nameless) {
      return parser_error(context, span,
                          "You cannot use nameless parameters in a function "
                          "that is not extern");
    }
  }

  Ast *block = OK(parse_block(lexer, context));
  END_SPAN();

  Ast *function = ast_alloc(context, AST_FUNCTION, span);
  function->function.block = block;
  function->function.parameters = parameters;
  function->function.return_type = return_type;
  function->function.name = name;

#if 0  
  print_ast_function_header(function);
#endif

  return function;
}

Ast *parse_extern(Lexer *lexer, Context *context) {
  BEGIN_SPAN(EXPECT(TOKEN_EXTERN));
  const char *name;
  Parameter_list parameters = {0};
  const char *return_type;
  Span header_span;

  if (!parse_function_header(lexer, context, &name, &parameters, &return_type,
                             &header_span)) {
    return NULL;
  }
  END_SPAN();

  Ast *extern_ast = ast_alloc(context, AST_EXTERN, span);

  extern_ast->extern_function.name = name;
  extern_ast->extern_function.parameters = parameters;
  extern_ast->extern_function.return_type = return_type;

  return extern_ast;
}

Ast *parse_variable(Lexer *lexer, Context *context) {
  BEGIN_SPAN(lexer_peek(lexer));
  Token name = EXPECT(TOKEN_IDENTIFIER);
  Ast *type = parse_type(lexer, context);
  EXPECT(TOKEN_ASSIGN);
  Ast *expression = OK(parse_expression(lexer, context));
  END_SPAN();
  Ast *var = ast_alloc(context, AST_VARIABLE, span);
  var->variable.type = type;
  var->variable.name = name.value;
  var->variable.initializer = expression;
  return var;
}

Ast *parse_aggregate_initializer(Lexer *lexer, Context *context) {
  BEGIN_SPAN(EXPECT(TOKEN_LCURLY));

  Ast_Ptr_list values = {0};
  string_list keys = {0};

  // used with the (long named) macro to maintain homogenous lists.
  // see the macro definition for more info.
  bool parsing_only_values = false, set = false;

  while (true) {
    Token peeked = lexer_peek(lexer);
    if (peeked.type == TOKEN_RCURLY) {
      break;
    }

    Token one_ahead = lexer_lookahead(lexer, 1);

    if (one_ahead.type == TOKEN_ASSIGN) {
      // key = value
      AGGREGATE_INITIALIZER_SET_CHECK_PARSING_KEY_VALUES(false);
      Token key = EXPECT(TOKEN_IDENTIFIER);
      LIST_PUSH(keys, key.value);
      LIST_PUSH(values, OK(parse_expression(lexer, context)));
    } else {
      // just values
      AGGREGATE_INITIALIZER_SET_CHECK_PARSING_KEY_VALUES(true);
      LIST_PUSH(values, OK(parse_expression(lexer, context)));
    }

    if (lexer_next(lexer) != TOKEN_RCURLY) {
      EXPECT(TOKEN_COMMA);
    }
  }

  EXPECT(TOKEN_RCURLY);
  END_SPAN();
  Ast *aggregate = ast_alloc(context, AST_AGGREGATE_INITIALIZER, span);
  aggregate->aggregate_initializer.keys = keys;
  aggregate->aggregate_initializer.values = values;
  return aggregate;
}

Ast *parse_struct(Lexer *lexer, Context *context) {
  BEGIN_SPAN(EXPECT(TOKEN_STRUCT));
  Ast_Struct_Member_list members = {0};
  Token name_token = EXPECT(TOKEN_IDENTIFIER);
  EXPECT(TOKEN_LCURLY);

  while (true) {
    if (lexer_next(lexer) == TOKEN_RCURLY) {
      break;
    }

    Ast_Struct_Member member = {0};
    member.name = EXPECT(TOKEN_IDENTIFIER).value;
    member.type = parse_type(lexer, context);
    LIST_PUSH(members, member);

    if (lexer_next(lexer) != TOKEN_RCURLY) {
      EXPECT(TOKEN_COMMA);
    }
  }

  EXPECT(TOKEN_RCURLY);
  END_SPAN()
  Ast *ast = ast_alloc(context, AST_STRUCT, span);
  ast->$struct.members = members;
  ast->$struct.name = name_token.value;
  return ast;
}

Operator parse_operator(Lexer *lexer, Context *context,
                        Expression_Type expr_type, Ast **error) {
  (void)context;
  (void)error; // this function does not report errors
  Token tok = lexer_peek(lexer);
  Token next = lexer_lookahead(lexer, 1);

  switch (expr_type) {
  case EXPR_UNARY:
    switch (tok.type) {
    case TOKEN_BIT_AND:
      lexer_eat(lexer);
      return OPERATOR_ADDRESS_OF;
    case TOKEN_MUL:
      lexer_eat(lexer);
      return OPERATOR_DEREFERENCE;
    case TOKEN_LOGICAL_NOT:
      lexer_eat(lexer);
      return OPERATOR_LOGICAL_NOT;
    case TOKEN_BIT_NOT:
      lexer_eat(lexer);
      return OPERATOR_BIT_NOT;
    case TOKEN_MINUS:
      lexer_eat(lexer);
      return OPERATOR_NEGATE;
    case TOKEN_PLUS: /* unary + not supported */
      return OPERATOR_NONE;
    default:
      return OPERATOR_NONE;
    }

  case EXPR_BINARY:
    switch (tok.type) {
    case TOKEN_ASSIGN:
      lexer_eat(lexer);
      return OPERATOR_ASSIGN;

    case TOKEN_LOGICAL_OR:
      lexer_eat(lexer);
      return OPERATOR_LOGICAL_OR;
    case TOKEN_LOGICAL_AND:
      lexer_eat(lexer);
      return OPERATOR_LOGICAL_AND;

    case TOKEN_BIT_OR:
      if (next.type == TOKEN_ASSIGN) {
        lexer_eat(lexer);
        lexer_eat(lexer);
        return OPERATOR_BIT_OR_ASSIGN;
      }
      lexer_eat(lexer);
      return OPERATOR_BIT_OR;

    case TOKEN_BIT_AND:
      if (next.type == TOKEN_ASSIGN) {
        lexer_eat(lexer);
        lexer_eat(lexer);
        return OPERATOR_BIT_AND_ASSIGN;
      }
      lexer_eat(lexer);
      return OPERATOR_BIT_AND;

    case TOKEN_XOR:
      /* no xor-assign in Operator enum */
      lexer_eat(lexer);
      return OPERATOR_XOR;

    case TOKEN_EQUALS:
      lexer_eat(lexer);
      return OPERATOR_EQUALS;
    case TOKEN_NOT_EQUALS:
      lexer_eat(lexer);
      return OPERATOR_NOT_EQUALS;

    case TOKEN_LESS:
      if (next.type == TOKEN_ASSIGN) {
        lexer_eat(lexer);
        lexer_eat(lexer);
        return OPERATOR_LESS_EQUAL;
      }
      lexer_eat(lexer);
      return OPERATOR_LESS;

    case TOKEN_GREATER:
      if (next.type == TOKEN_ASSIGN) {
        lexer_eat(lexer);
        lexer_eat(lexer);
        return OPERATOR_GREATER_EQUAL;
      }
      lexer_eat(lexer);
      return OPERATOR_GREATER;

    case TOKEN_SHIFT_LEFT:
      if (next.type == TOKEN_ASSIGN) {
        lexer_eat(lexer);
        lexer_eat(lexer);
        return OPERATOR_SHIFT_LEFT_ASSIGN;
      }
      lexer_eat(lexer);
      return OPERATOR_SHIFT_LEFT;

    case TOKEN_SHIFT_RIGHT:
      if (next.type == TOKEN_ASSIGN) {
        lexer_eat(lexer);
        lexer_eat(lexer);
        return OPERATOR_SHIFT_RIGHT_ASSIGN;
      }
      lexer_eat(lexer);
      return OPERATOR_SHIFT_RIGHT;

    case TOKEN_PLUS:
      if (next.type == TOKEN_ASSIGN) {
        lexer_eat(lexer);
        lexer_eat(lexer);
        return OPERATOR_PLUS_ASSIGN;
      }
      lexer_eat(lexer);
      return OPERATOR_ADD;

    case TOKEN_MINUS:
      if (next.type == TOKEN_ASSIGN) {
        lexer_eat(lexer);
        lexer_eat(lexer);
        return OPERATOR_MINUS_ASSIGN;
      }
      lexer_eat(lexer);
      return OPERATOR_SUB;

    case TOKEN_MUL:
      if (next.type == TOKEN_ASSIGN) {
        lexer_eat(lexer);
        lexer_eat(lexer);
        return OPERATOR_STAR_ASSIGN;
      }
      lexer_eat(lexer);
      return OPERATOR_MUL;

    case TOKEN_SLASH:
      if (next.type == TOKEN_ASSIGN) {
        lexer_eat(lexer);
        lexer_eat(lexer);
        return OPERATOR_SLASH_ASSIGN;
      }
      lexer_eat(lexer);
      return OPERATOR_DIV;

    default:
      return OPERATOR_NONE;
    }

  case EXPR_POSTFIX:
    switch (tok.type) {
    case TOKEN_LBRACKET:
      lexer_eat(lexer);
      return OPERATOR_INDEX;
    case TOKEN_DOT: /* handled by parse_postfix */
      return OPERATOR_NONE;
    default:
      return OPERATOR_NONE;
    }

  default:
    return OPERATOR_NONE;
  }
}
Precedence get_precedence(Operator op, bool *is_valid_operator) {
  *is_valid_operator = true;
  switch (op) {
  case OPERATOR_ASSIGN:
  case OPERATOR_PLUS_ASSIGN:
  case OPERATOR_MINUS_ASSIGN:
  case OPERATOR_STAR_ASSIGN:
  case OPERATOR_SLASH_ASSIGN:
  case OPERATOR_BIT_OR_ASSIGN:
  case OPERATOR_BIT_AND_ASSIGN:
  case OPERATOR_SHIFT_LEFT_ASSIGN:
  case OPERATOR_SHIFT_RIGHT_ASSIGN:
    return PREC_ASSIGNMENT;
  case OPERATOR_LOGICAL_OR:
    return PREC_LOGICAL_OR;
  case OPERATOR_LOGICAL_AND:
    return PREC_LOGICAL_AND;
  case OPERATOR_BIT_OR:
    return PREC_BIT_OR;
  case OPERATOR_XOR: // Added XOR
    return PREC_XOR;
  case OPERATOR_BIT_AND:
    return PREC_BIT_AND;
  case OPERATOR_EQUALS:
  case OPERATOR_NOT_EQUALS:
    return PREC_EQUALITY;
  case OPERATOR_LESS:
  case OPERATOR_GREATER:
  case OPERATOR_LESS_EQUAL:
  case OPERATOR_GREATER_EQUAL:
    return PREC_RELATIONAL;
  case OPERATOR_SHIFT_LEFT:
  case OPERATOR_SHIFT_RIGHT:
    return PREC_SHIFT;
  case OPERATOR_ADD:
  case OPERATOR_SUB:
    return PREC_TERM;
  case OPERATOR_MUL:
  case OPERATOR_DIV:
    return PREC_FACTOR;
  case OPERATOR_LOGICAL_NOT:
  case OPERATOR_BIT_NOT:
  case OPERATOR_NEGATE:
  case OPERATOR_DEREFERENCE:
  case OPERATOR_ADDRESS_OF:
    return PREC_UNARY;
  case OPERATOR_INDEX:
    return PREC_POSTFIX;
  case OPERATOR_NONE:
    *is_valid_operator = false;
    return PREC_NONE;
  default:
    *is_valid_operator = false;
    return PREC_NONE;
  }
}
