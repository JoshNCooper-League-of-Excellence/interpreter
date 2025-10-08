#include "parser.h"
#include "ast.h"
#include "binding.h"
#include "lexer.h"
#include <limits.h>

#define EXPECT($tok)                                                           \
  ({                                                                           \
    Token tok = lexer_peek(lexer);                                             \
    if (tok.type != $tok) {                                                    \
      char *error_msg;                                                         \
      asprintf(&error_msg,                                                     \
               "unexpected token at %s:%zu:%zu, expected %d, got %d\n",        \
               lexer->filename, tok.span.line, tok.span.col, $tok, tok.type);  \
      __asm("int3");                                                           \
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
  ast_alloc(context, AST_PROGRAM, lexer_span(lexer));
  while (!lexer_next_is(lexer, TOKEN_EOF)) {
    switch (lexer_next(lexer)) {
    case TOKEN_IDENTIFIER: {
      return parse_function(lexer, context);
    }

    default:
      return parser_error(context, lexer_span(lexer),
                          "Unexpected token at top level.", true);
      break;
    }
  }

  return parser_error(context, lexer_span(lexer), "Unexpected end of input",
                      true);
}

Ast *parse_block(Lexer *lexer, Context *context) {
  BEGIN_SPAN(EXPECT(TOKEN_LCURLY))
  Ast_Ptr_list statements;

  while (true) {
    Token peeked = lexer_peek(lexer);
    if (peeked.type == TOKEN_RCURLY) {
      break;
    }
    switch (peeked.type) {
    case TOKEN_SEMI:
      // ignore extraneous semis
      lexer_eat(lexer);
      break;
    case TOKEN_IDENTIFIER:
      LIST_PUSH(statements, OK(parse_expression(lexer, context)));
      break;
    case TOKEN_VAR:
      LIST_PUSH(statements, OK(parse_variable(lexer, context)));
      break;
    case TOKEN_RETURN:
      if (lexer_next_is(lexer, TOKEN_SEMI)) {
        END_SPAN()
        Ast *return_expr = ast_alloc(context, AST_RETURN, span);
        return_expr->return_value = nullptr;
      }
      Ast *expression = parse_expression(lexer, context);
      END_SPAN();
      Ast *return_expr = ast_alloc(context, AST_RETURN, span);
      return_expr->return_value = expression;
      return return_expr;
      break;
    case TOKEN_EOF:
      return parser_error(context, span,
                          "Unexpected end of input while parsing block", true);
    default:
      // easily propagate formatted error.
      // -2 doesn't exist.
      EXPECT(-2);
    }
  }

  EXPECT(TOKEN_RCURLY);
  END_SPAN();

  Ast *block = ast_alloc(context, AST_BLOCK, span);
  block->block = statements;
  return block;
}

Ast *parse_primary(Lexer *lexer, Context *context) {
  Token peeked = lexer_peek(lexer);
  BEGIN_SPAN(peeked);

  switch (peeked.type) {
  case TOKEN_INTEGER: {
    lexer_eat(lexer);
    END_SPAN()
    Ast *integer = ast_alloc(context, AST_LITERAL, span);
    integer->literal.tag = INTEGER;
    integer->literal.value = peeked.value;
    return integer;
  } break;
  case TOKEN_STRING: {
    lexer_eat(lexer);
    END_SPAN()
    Ast *integer = ast_alloc(context, AST_LITERAL, span);
    integer->literal.tag = STRING;
    integer->literal.value = peeked.value;
    return integer;
  } break;
  default:
    char *buf;
    asprintf(&buf, "unexpected token when parsing literal: %d", peeked.type);
    return parser_error(context, span, buf, true);
  }
}

Ast *parse_binary(Lexer *lexer, Context *context, Precedence precedence) {
  BEGIN_SPAN(lexer_peek(lexer));
  Ast *left = OK(parse_primary(lexer, context));

  while (true) {
    Token op = lexer_peek(lexer);
    Precedence op_prec = get_precedence(op.type);

    if (op_prec < precedence) {
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

Ast *parse_expression(Lexer *lexer, Context *context) {
  return parse_binary(lexer, context, PREC_NONE);
}

Ast *parse_function(Lexer *lexer, Context *context) {
  Token identifier = lexer_eat(lexer);
  BEGIN_SPAN(identifier);
  span = identifier.span;
  EXPECT(TOKEN_COLON);
  EXPECT(TOKEN_COLON);
  EXPECT(TOKEN_LPAREN);

  Parameter_list parameters = {0};
  while (true) {
    Token peeked = lexer_peek(lexer);
    if (peeked.type == TOKEN_EOF || peeked.type == TOKEN_RPAREN) {
      break;
    }

    Token name = EXPECT(TOKEN_IDENTIFIER);
    Token type = EXPECT(TOKEN_IDENTIFIER);
    Parameter parameter = {name.value, type.value};
    LIST_PUSH(parameters, parameter);

    if (lexer_next(lexer) != TOKEN_RPAREN) {
      EXPECT(TOKEN_COMMA);
    }
  }

  EXPECT(TOKEN_RPAREN);
  Token returns = EXPECT(TOKEN_IDENTIFIER);
  Ast *block = OK(parse_block(lexer, context));
  END_SPAN();

  Ast *function = ast_alloc(context, AST_FUNCTION, span);
  function->function = (typeof(function->function)){
      .block = block,
      .parameters = parameters,
      .return_type = returns.value,
  };
  return function;
}

Ast *parse_variable(Lexer *lexer, Context *context) {
  BEGIN_SPAN(EXPECT(TOKEN_VAR));
  Token identifier = EXPECT(TOKEN_IDENTIFIER);
  EXPECT(TOKEN_ASSIGN);
  Ast *expression = parse_expression(lexer, context);
  EXPECT(TOKEN_SEMI);
  END_SPAN();
  Ast *var = ast_alloc(context, AST_VARIABLE, span);
  var->variable.name = identifier.value;
  var->variable.initializer = expression;
  return var;
}