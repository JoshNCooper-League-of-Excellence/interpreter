#ifndef LEXER_H
#define LEXER_H

#define _GNU_SOURCE
#ifndef __USE_MISC
#define __USE_MISC
#endif

#include "list.h"
#include <assert.h>
#include <ctype.h>
#include <stddef.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
  TOKEN_EOF = -1,

  TOKEN_IDENTIFIER,
  TOKEN_INTEGER,
  TOKEN_STRING,

  TOKEN_COLON,

  TOKEN_LPAREN,
  TOKEN_RPAREN,

  TOKEN_LCURLY,
  TOKEN_RCURLY,

  TOKEN_SEMI,
  TOKEN_COMMA,

  TOKEN_EXTERN,
  TOKEN_RETURN,

  TOKEN_PLUS,
  TOKEN_MINUS,
  TOKEN_SLASH,
  TOKEN_STAR,
  TOKEN_ASSIGN,

  TOKEN_STRUCT,
  TOKEN_DOT,
} Token_Type;

static inline const char *token_type_to_string(Token_Type type) {
  switch (type) {
  case TOKEN_EOF:
    return "EOF";
  case TOKEN_IDENTIFIER:
    return "IDENTIFIER";
  case TOKEN_INTEGER:
    return "INTEGER";
  case TOKEN_STRING:
    return "STRING";
  case TOKEN_COLON:
    return "COLON";
  case TOKEN_LPAREN:
    return "LPAREN";
  case TOKEN_RPAREN:
    return "RPAREN";
  case TOKEN_LCURLY:
    return "LCURLY";
  case TOKEN_RCURLY:
    return "RCURLY";
  case TOKEN_SEMI:
    return "SEMI";
  case TOKEN_COMMA:
    return "COMMA";
  case TOKEN_RETURN:
    return "RETURN";
  case TOKEN_PLUS:
    return "PLUS";
  case TOKEN_MINUS:
    return "MINUS";
  case TOKEN_SLASH:
    return "SLASH";
  case TOKEN_STAR:
    return "STAR";
  case TOKEN_ASSIGN:
    return "ASSIGN";
  default:
    return "UNKNOWN";
  }
}

typedef struct {
  size_t line, col, length, start;
} Span;

extern const char *CURRENTLY_COMPILING_FILE_NAME;

static inline char *lexer_span_to_string(Span span) {
  char *buffer = malloc(64);
  snprintf(buffer, 64, "%s:%zu:%zu", CURRENTLY_COMPILING_FILE_NAME, span.line,
           span.col);
  return buffer;
}

typedef struct {
  const char *value;
  bool has_value;
  Token_Type type;
  Span span;
} Token;

DEFINE_LIST(Token);

typedef struct {
  size_t pos, col, line, length;
  const char *filename;
  const char *input;
  Token_list lookahead;
} Lexer;

static inline void lexer_init(Lexer *lexer, const char *filename) {
  FILE *file = fopen(filename, "r");

  if (!file) {
    fprintf(stderr, "unable to open file '%s'\n", filename);
    exit(1);
  }

  fseek(file, 0, SEEK_END);
  int length = ftell(file);
  fseek(file, 0, SEEK_SET);
  char *buffer = malloc(sizeof(char) * length);
  fread(buffer, 1, length, file);
  fclose(file);

  lexer->length = length;
  lexer->input = buffer;
  lexer->filename = filename;
  lexer->pos = 0;
  lexer->col = 1;
  lexer->line = 1;
  lexer->lookahead = (Token_list){0};
}

static inline Token lexer_gettok(Lexer *lexer) {
  size_t start = lexer->pos;
  while (lexer->pos < lexer->length) {
    char c = lexer->input[lexer->pos];
    start = lexer->pos;

    // Single-line comment (// ...)
    if (c == '/' && lexer->pos + 1 < lexer->length &&
        lexer->input[lexer->pos + 1] == '/') {
      lexer->pos += 2;
      lexer->col += 2;
      while (lexer->pos < lexer->length && lexer->input[lexer->pos] != '\n') {
        lexer->pos++;
        lexer->col++;
      }
      if (lexer->pos < lexer->length && lexer->input[lexer->pos] == '\n') {
        lexer->pos++;
        lexer->line++;
        lexer->col = 1;
      }
      continue;
    }

    // Multi-line comment (/* ... */)
    if (c == '/' && lexer->pos + 1 < lexer->length &&
        lexer->input[lexer->pos + 1] == '*') {
      lexer->pos += 2;
      lexer->col += 2;
      while (lexer->pos < lexer->length) {
        if (lexer->input[lexer->pos] == '*' && lexer->pos + 1 < lexer->length &&
            lexer->input[lexer->pos + 1] == '/') {
          lexer->pos += 2;
          lexer->col += 2;
          break;
        }
        if (lexer->input[lexer->pos] == '\n') {
          lexer->line++;
          lexer->col = 1;
        } else {
          lexer->col++;
        }
        lexer->pos++;
      }
      continue;
    }

    if (c == '"') {
      size_t begin_line = lexer->line;
      size_t begin_col = lexer->col;
      lexer->pos++;
      lexer->col++;
      size_t start = lexer->pos;

      while (lexer->pos < lexer->length && lexer->input[lexer->pos] != '"') {
        if (lexer->input[lexer->pos] == '\n') {
          lexer->line++;
          lexer->col = 1;
        } else {
          lexer->col++;
        }
        lexer->pos++;
      }

      if (lexer->pos >= lexer->length) {
        fprintf(stderr, "unterminated string literal at %s:%zu:%zu\n",
                lexer->filename, begin_line, begin_col);
        exit(1);
      }

      size_t len = lexer->pos - start;
      char *str = malloc(len + 1);
      strncpy(str, lexer->input + start, len);
      str[len] = '\0';
      lexer->pos++; // skip closing quote
      lexer->col++;
      return (Token){str, true, TOKEN_STRING,
                     .span = {begin_line, begin_col, len + 2, start - 1}};
    }

    if (isalpha(c)) {
      size_t begin_line = lexer->line;
      size_t begin_col = lexer->col;
      while (lexer->pos < lexer->length && (isalnum(lexer->input[lexer->pos]) ||
                                            lexer->input[lexer->pos] == '_')) {
        lexer->pos++;
        lexer->col++;
      }
      size_t len = lexer->pos - start;

      struct {
        const char *kw;
        Token_Type type;
        size_t len;
      } keywords[] = {
          {"extern", TOKEN_EXTERN, 6},
          {"struct", TOKEN_STRUCT, 6},
          {"return", TOKEN_RETURN, 6},
      };

      for (size_t i = 0; i < sizeof(keywords) / sizeof(keywords[0]); ++i) {
        if (len == keywords[i].len &&
            strncmp(lexer->input + start, keywords[i].kw, keywords[i].len) ==
                0) {
          return (Token){nullptr, false, keywords[i].type,
                         .span = {begin_line, begin_col, len, start}};
        }
      }
      char *ident = malloc(len + 1);
      strncpy(ident, lexer->input + start, len);
      ident[len] = '\0';
      return (Token){ident, true, TOKEN_IDENTIFIER,
                     .span = {begin_line, begin_col, len, start}};
    } else if (isdigit(c)) {
      size_t start = lexer->pos;
      size_t begin_line = lexer->line;
      size_t begin_col = lexer->col;
      while (lexer->pos < lexer->length && isdigit(lexer->input[lexer->pos])) {
        lexer->pos++;
        lexer->col++;
      }
      size_t len = lexer->pos - start;
      char *num = malloc(len + 1);
      strncpy(num, lexer->input + start, len);
      num[len] = '\0';
      return (Token){num, true, TOKEN_INTEGER,
                     .span = {begin_line, begin_col, len, start}};
    } else if (ispunct(c)) {
      size_t begin_line = lexer->line;
      size_t begin_col = lexer->col;
      lexer->pos++;
      lexer->col++;
      switch (c) {
      case '+':
        return (Token){nullptr, false, TOKEN_PLUS,
                       .span = {begin_line, begin_col, 1, start}};
      case '-':
        return (Token){nullptr, false, TOKEN_MINUS,
                       .span = {begin_line, begin_col, 1, start}};
      case '/':
        return (Token){nullptr, false, TOKEN_SLASH,
                       .span = {begin_line, begin_col, 1, start}};
      case '*':
        return (Token){nullptr, false, TOKEN_STAR,
                       .span = {begin_line, begin_col, 1, start}};
      case '=':
        return (Token){nullptr, false, TOKEN_ASSIGN,
                       .span = {begin_line, begin_col, 1, start}};
      case '.':
        return (Token){nullptr, false, TOKEN_ASSIGN,
                       .span = {begin_line, begin_col, 1, start}};
      case ':':
        return (Token){nullptr, false, TOKEN_COLON,
                       .span = {begin_line, begin_col, 1, start}};
      case ';':
        return (Token){nullptr, false, TOKEN_SEMI,
                       .span = {begin_line, begin_col, 1, start}};
      case ',':
        return (Token){nullptr, false, TOKEN_COMMA,
                       .span = {begin_line, begin_col, 1, start}};
      case '(':
        return (Token){nullptr, false, TOKEN_LPAREN,
                       .span = {begin_line, begin_col, 1, start}};
      case ')':
        return (Token){nullptr, false, TOKEN_RPAREN,
                       .span = {begin_line, begin_col, 1, start}};
      case '{':
        return (Token){nullptr, false, TOKEN_LCURLY,
                       .span = {begin_line, begin_col, 1, start}};
      case '}':
        return (Token){nullptr, false, TOKEN_RCURLY,
                       .span = {begin_line, begin_col, 1, start}};
      default:
        break;
      }
    } else if (isspace(c)) {
      if (c == '\n') {
        lexer->line++;
        lexer->col = 1;
      } else {
        lexer->col++;
      }
      lexer->pos++;
      continue;
    } else {
      fprintf(stderr, "unexpected character in input ('%c') at %s:%zu:%zu", c,
              lexer->filename, lexer->line, lexer->col);
      lexer->pos++;
      lexer->col++;
      continue;
    }
  }
  return (Token){.type = TOKEN_EOF,
                 .span = {lexer->line, lexer->col, 0, start}};
}

#define LEXER_BUFFER_SIZE 8

static inline void lexer_fill_buffer(Lexer *lexer) {
  while (lexer->lookahead.length < LEXER_BUFFER_SIZE) {
    Token tok = lexer_gettok(lexer);
    LIST_PUSH(lexer->lookahead, tok);
  }
}

static inline Token lexer_peek(Lexer *lexer) {
  lexer_fill_buffer(lexer);
  return lexer->lookahead.data[0];
}

static inline Token lexer_eat(Lexer *lexer) {
  lexer_fill_buffer(lexer);
  Token tok = lexer->lookahead.data[0];
  LIST_REMOVE(lexer->lookahead, 0);
  return tok;
}

static inline Token lexer_expect(Lexer *lexer, Token_Type expected,
                                 char **error) {
  lexer_fill_buffer(lexer);
  Token tok = lexer->lookahead.data[0];
  if (tok.type != expected) {
    asprintf(error, "unexpected token at %s:%zu:%zu, expected %d, got %d\n",
             lexer->filename, tok.span.line, tok.span.col, expected, tok.type);
    return (Token){.type = TOKEN_EOF, .span = tok.span};
  }
  LIST_REMOVE(lexer->lookahead, 0);
  return tok;
}

static inline bool lexer_next_is(Lexer *lexer, Token_Type expected) {
  return lexer_peek(lexer).type == expected;
}

static inline bool lexer_next_is_and_not_eof(Lexer *lexer,
                                             Token_Type expected) {
  Token tok = lexer_peek(lexer);
  return tok.type == expected && tok.type != TOKEN_EOF;
}

static inline Token_Type lexer_next(Lexer *lexer) {
  return lexer_peek(lexer).type;
}

static inline Span lexer_span(Lexer *lexer) { return lexer_peek(lexer).span; }

static inline Token lexer_lookahead(Lexer *lexer, size_t n_tokens) {
  lexer_fill_buffer(lexer);
  if (n_tokens == 0 || n_tokens > lexer->lookahead.length)
    return (Token){.type = TOKEN_EOF, .span = lexer->lookahead.data[0].span};
  return lexer->lookahead.data[n_tokens];
}

#endif