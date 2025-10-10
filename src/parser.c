#include "parser.h"
#include "ast.h"
#include "binding.h"
#include "lexer.h"
#include "list.h"
#include <limits.h>

void print_ast_function_header(Ast *function) {
  printf("%s :: (", function->function.name);
  LIST_FOREACH(function->function.parameters, param) {
    printf("%s %s", param.identifier, param.type);
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
      return parser_error(context, lexer_span(lexer), error_msg, true);        \
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

Ast *parser_error(Context *context, Span span, const char *message,
                  bool fatal) {
  fprintf(stderr, "%s:%zu:%zu: %s\n", CURRENTLY_COMPILING_FILE_NAME, span.line,
          span.col, message);
  Ast *ast = ast_alloc(context, AST_ERROR, span);
  ast->error.message = message;
  ast->error.fatal = fatal;
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
      return parser_error(context, lexer_span(lexer), buf, true);
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

    switch (peeked.type) {
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
                          "Unexpected end of input while parsing block", true);
    default:
      // easily propagate formatted error.
      // -2 doesn't exist.
      EXPECT(-2);
      break;
    }

    EXPECT(TOKEN_SEMI);
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
    integer->literal.tag = INTEGER;
    integer->literal.value = peeked.value;
    return integer;
  }
  case TOKEN_STRING: {
    lexer_eat(lexer);
    END_SPAN()
    Ast *integer = ast_alloc(context, AST_LITERAL, span);
    integer->literal.tag = STRING;
    integer->literal.value = peeked.value;
    return integer;
  }
  default:
    lexer_eat(lexer);
    char *buf;
    asprintf(&buf, "unexpected token when parsing literal: %s",
             token_type_to_string(peeked.type));
    return parser_error(context, span, buf, true);
  }
}

Ast *parse_binary(Lexer *lexer, Context *context, Precedence precedence) {
  BEGIN_SPAN(lexer_peek(lexer));
  Ast *left = OK(parse_postfix(lexer, context));
  while (true) {
    Token op = lexer_peek(lexer);
    bool is_valid_operator = false;
    Precedence op_prec = get_precedence(op.type, &is_valid_operator);

    if (op_prec < precedence || !is_valid_operator) {
      break;
    }

    lexer_eat(lexer);
    Ast *right = OK(parse_binary(lexer, context, op_prec + 1));

    END_SPAN();
    Ast *bin = ast_alloc(context, AST_BINARY, span);
    bin->binary.left = left;
    bin->binary.right = right;
    bin->binary.op = op.type;
    left = bin;
  }
  return left;
}

Ast *parse_identifier(Lexer *lexer, Context *context) {
  Token one_ahead = lexer_lookahead(lexer, 1);
  Token two_ahead = lexer_lookahead(lexer, 2);
  if (two_ahead.type == TOKEN_ASSIGN) {
    return parse_variable(lexer, context);
  } else if (two_ahead.type == TOKEN_COLON) {
    return parse_function(lexer, context);
  } else if (one_ahead.type == TOKEN_LPAREN) {
    return parse_expression(lexer, context); // Function call
  } else if (one_ahead.type == TOKEN_ASSIGN) {
    return parse_expression(lexer, context);
  } else {
    return parser_error(context, lexer_span(lexer),
                        "invalid identifier statement. expected variable or "
                        "function declaration",
                        true);
  }
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
    if (peeked.type == TOKEN_EOF || peeked.type == TOKEN_RPAREN) {
      break;
    }

    Token param_name = EXPECT(TOKEN_IDENTIFIER);
    Token param_type = EXPECT(TOKEN_IDENTIFIER);
    Parameter parameter = {param_name.value, param_type.value};
    LIST_PUSH(*parameters, parameter);

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
  Ast *type = parse_type(lexer, context);
  Token name = EXPECT(TOKEN_IDENTIFIER);
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
