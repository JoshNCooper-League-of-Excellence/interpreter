#ifndef LEXER_H
#define LEXER_H

#include <linux/limits.h>
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
  TOKEN_EOF = 0,

  TOKEN_IDENTIFIER,

  TOKEN_INTEGER,
  TOKEN_STRING,

  TOKEN_TRUE,
  TOKEN_FALSE,

  TOKEN_COLON,

  TOKEN_LPAREN,
  TOKEN_RPAREN,

  TOKEN_LCURLY,
  TOKEN_RCURLY,

  TOKEN_SEMI,
  TOKEN_COMMA,

  TOKEN_EXTERN,
  TOKEN_RETURN,
  TOKEN_CONTINUE,
  TOKEN_BREAK,
  TOKEN_GOTO,

  TOKEN_IF,
  TOKEN_ELSE,
  TOKEN_WHILE,
  TOKEN_FOR,

  TOKEN_PLUS,
  TOKEN_MINUS,
  TOKEN_SLASH,
  TOKEN_STAR,
  TOKEN_ASSIGN,
  TOKEN_XOR,

  TOKEN_STRUCT,
  TOKEN_DOT,

  TOKEN_SHIFT_LEFT,
  TOKEN_SHIFT_RIGHT,

  // relationals
  TOKEN_LOGICAL_AND,
  TOKEN_LOGICAL_OR,
  TOKEN_LOGICAL_NOT,
  TOKEN_EQUALS,
  TOKEN_NOT_EQUALS,

  TOKEN_GREATER,
  TOKEN_LESS,

  TOKEN_BIT_AND,
  TOKEN_BIT_OR,
  TOKEN_BIT_NOT,

  TOKEN_PERCENT,

  TOKEN_LBRACKET,
  TOKEN_RBRACKET,
} Token_Type;

static inline const char *token_type_to_string(Token_Type type) {
  static const char *lut[] = {[TOKEN_EOF] = "EOF",
                              [TOKEN_IDENTIFIER] = "IDENTIFIER",
                              [TOKEN_INTEGER] = "INTEGER",
                              [TOKEN_STRING] = "STRING",
                              [TOKEN_TRUE] = "TRUE",
                              [TOKEN_FALSE] = "FALSE",
                              [TOKEN_COLON] = "COLON",
                              [TOKEN_LPAREN] = "LPAREN",
                              [TOKEN_RPAREN] = "RPAREN",
                              [TOKEN_LCURLY] = "LCURLY",
                              [TOKEN_RCURLY] = "RCURLY",
                              [TOKEN_SEMI] = "SEMI",
                              [TOKEN_COMMA] = "COMMA",
                              [TOKEN_EXTERN] = "EXTERN",
                              [TOKEN_RETURN] = "RETURN",
                              [TOKEN_IF] = "IF",
                              [TOKEN_ELSE] = "ELSE",
                              [TOKEN_WHILE] = "WHILE",
                              [TOKEN_PLUS] = "PLUS",
                              [TOKEN_MINUS] = "MINUS",
                              [TOKEN_SLASH] = "SLASH",
                              [TOKEN_STAR] = "STAR",
                              [TOKEN_ASSIGN] = "ASSIGN",
                              [TOKEN_XOR] = "XOR",
                              [TOKEN_STRUCT] = "STRUCT",
                              [TOKEN_DOT] = "DOT",
                              [TOKEN_SHIFT_LEFT] = "SHIFT_LEFT",
                              [TOKEN_SHIFT_RIGHT] = "SHIFT_RIGHT",
                              [TOKEN_LOGICAL_AND] = "LOGICAL_AND",
                              [TOKEN_LOGICAL_OR] = "LOGICAL_OR",
                              [TOKEN_LOGICAL_NOT] = "LOGICAL_NOT",
                              [TOKEN_EQUALS] = "EQUALS",
                              [TOKEN_NOT_EQUALS] = "NOT_EQUALS",
                              [TOKEN_GREATER] = "GREATER",
                              [TOKEN_LESS] = "LESS",
                              [TOKEN_BIT_AND] = "BIT_AND",
                              [TOKEN_BIT_OR] = "BIT_OR",
                              [TOKEN_BIT_NOT] = "BIT_NOT",
                              [TOKEN_PERCENT] = "PERCENT",
                              [TOKEN_LBRACKET] = "LBRACKET",
                              [TOKEN_RBRACKET] = "RBRACKET"};
  if (type < 0 || type >= (int)(sizeof(lut) / sizeof(lut[0])) || lut[type] == NULL)
    return "UNKNOWN";
  return lut[type];
}

typedef struct {
  size_t line, col, length, start;
} Span;

extern char CURRENTLY_COMPILING_FILE_NAME[PATH_MAX];

static inline char *lexer_span_to_string(Span span) {
  char *buffer = malloc(64);
  snprintf(buffer, 64, "%s:%zu:%zu", CURRENTLY_COMPILING_FILE_NAME, span.line, span.col);
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
    if (c == '/' && lexer->pos + 1 < lexer->length && lexer->input[lexer->pos + 1] == '/') {
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
    if (c == '/' && lexer->pos + 1 < lexer->length && lexer->input[lexer->pos + 1] == '*') {
      lexer->pos += 2;
      lexer->col += 2;
      while (lexer->pos < lexer->length) {
        if (lexer->input[lexer->pos] == '*' && lexer->pos + 1 < lexer->length && lexer->input[lexer->pos + 1] == '/') {
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
        fprintf(stderr, "unterminated string literal at %s:%zu:%zu\n", lexer->filename, begin_line, begin_col);
        exit(1);
      }

      size_t len = lexer->pos - start;
      char *str = malloc(len + 1);
      strncpy(str, lexer->input + start, len);
      str[len] = '\0';
      lexer->pos++; // skip closing quote
      lexer->col++;
      return (Token){str, true, TOKEN_STRING, .span = {begin_line, begin_col, len + 2, start - 1}};
    }

    if (isalpha(c)) {
      size_t begin_line = lexer->line;
      size_t begin_col = lexer->col;
      while (lexer->pos < lexer->length && (isalnum(lexer->input[lexer->pos]) || lexer->input[lexer->pos] == '_')) {
        lexer->pos++;
        lexer->col++;
      }
      size_t len = lexer->pos - start;

      struct {
        const char *kw;
        Token_Type type;
        size_t len;
      } keywords[] = {{"extern", TOKEN_EXTERN, 6}, {"true", TOKEN_TRUE, 4},     {"false", TOKEN_FALSE, 5},
                      {"struct", TOKEN_STRUCT, 6}, {"return", TOKEN_RETURN, 6}, {"if", TOKEN_IF, 2},
                      {"else", TOKEN_ELSE, 4},     {"while", TOKEN_WHILE, 5},   {"for", TOKEN_FOR, 3},
                      {"continue", TOKEN_CONTINUE, 8}, {"break", TOKEN_BREAK, 5}, {"goto", TOKEN_GOTO, 4}
                    };

      for (size_t i = 0; i < sizeof(keywords) / sizeof(keywords[0]); ++i) {
        if (len == keywords[i].len && strncmp(lexer->input + start, keywords[i].kw, keywords[i].len) == 0) {
          return (Token){nullptr, false, keywords[i].type, .span = {begin_line, begin_col, len, start}};
        }
      }
      char *ident = malloc(len + 1);
      strncpy(ident, lexer->input + start, len);
      ident[len] = '\0';
      return (Token){ident, true, TOKEN_IDENTIFIER, .span = {begin_line, begin_col, len, start}};
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
      return (Token){num, true, TOKEN_INTEGER, .span = {begin_line, begin_col, len, start}};
    } else if (ispunct(c)) {
      size_t begin_line = lexer->line;
      size_t begin_col = lexer->col;
      lexer->pos++;
      lexer->col++;

      Token_Type type = TOKEN_EOF;
      switch (c) {
      case '&':
        if (lexer->pos < lexer->length && lexer->input[lexer->pos] == '&') {
          lexer->pos++;
          lexer->col++;
          type = TOKEN_LOGICAL_AND;
        } else {
          type = TOKEN_BIT_AND;
        }
        break;
      case '|':
        if (lexer->pos < lexer->length && lexer->input[lexer->pos] == '|') {
          lexer->pos++;
          lexer->col++;
          type = TOKEN_LOGICAL_OR;
        } else {
          type = TOKEN_BIT_OR;
        }
        break;
      case '[':
        type = TOKEN_LBRACKET;
        break;
      case ']':
        type = TOKEN_RBRACKET;
        break;
      case '!':
        if (lexer->pos < lexer->length && lexer->input[lexer->pos] == '=') {
          lexer->pos++;
          lexer->col++;
          type = TOKEN_NOT_EQUALS;
        } else {
          type = TOKEN_LOGICAL_NOT;
        }
        break;
      case '=':
        if (lexer->pos < lexer->length && lexer->input[lexer->pos] == '=') {
          lexer->pos++;
          lexer->col++;
          type = TOKEN_EQUALS;
        } else {
          type = TOKEN_ASSIGN;
        }
        break;
      case '<':
        if (lexer->pos < lexer->length && lexer->input[lexer->pos] == '<') {
          lexer->pos++;
          lexer->col++;
          type = TOKEN_SHIFT_LEFT;
        } else {
          type = TOKEN_LESS;
        }
        break;
      case '>':
        if (lexer->pos < lexer->length && lexer->input[lexer->pos] == '>') {
          lexer->pos++;
          lexer->col++;
          type = TOKEN_SHIFT_RIGHT;
        } else {
          type = TOKEN_GREATER;
        }
        break;
      case '~':
        type = TOKEN_BIT_NOT;
        break;
      case '%':
        type = TOKEN_PERCENT;
        break;
      case '+':
        type = TOKEN_PLUS;
        break;
      case '-':
        type = TOKEN_MINUS;
        break;
      case '/':
        type = TOKEN_SLASH;
        break;
      case '^':
        type = TOKEN_XOR;
        break;
      case '*':
        type = TOKEN_STAR;
        break;
      case '.':
        type = TOKEN_DOT;
        break;
      case ':':
        type = TOKEN_COLON;
        break;
      case ';':
        type = TOKEN_SEMI;
        break;
      case ',':
        type = TOKEN_COMMA;
        break;
      case '(':
        type = TOKEN_LPAREN;
        break;
      case ')':
        type = TOKEN_RPAREN;
        break;
      case '{':
        type = TOKEN_LCURLY;
        break;
      case '}':
        type = TOKEN_RCURLY;
        break;
      default:
        break;
      }
      if (type == TOKEN_EOF) {
        fprintf(stderr, "unknown operator in input ('%c') at %s:%zu:%zu", c, lexer->filename, lexer->line, lexer->col);
        lexer->pos++;
        lexer->col++;
        continue;
      }
      return (Token){nullptr, false, type, .span = {begin_line, begin_col, 1, start}};
    }
    if (isspace(c)) {
      if (c == '\n') {
        lexer->line++;
        lexer->col = 1;
      } else {
        lexer->col++;
      }
      lexer->pos++;
      continue;
    } else {
      fprintf(stderr, "unexpected character in input ('%c') at %s:%zu:%zu", c, lexer->filename, lexer->line, lexer->col);
      lexer->pos++;
      lexer->col++;
      continue;
    }
  }
  return (Token){.type = TOKEN_EOF, .span = {lexer->line, lexer->col, 0, start}};
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

static inline Token lexer_expect(Lexer *lexer, Token_Type expected, char **error) {
  lexer_fill_buffer(lexer);
  Token tok = lexer->lookahead.data[0];
  if (tok.type != expected) {
    asprintf(error, "unexpected token at %s:%zu:%zu, expected %d, got %d\n", lexer->filename, tok.span.line, tok.span.col,
             expected, tok.type);
    return (Token){.type = TOKEN_EOF, .span = tok.span};
  }
  LIST_REMOVE(lexer->lookahead, 0);
  return tok;
}

static inline bool lexer_next_is(Lexer *lexer, Token_Type expected) { return lexer_peek(lexer).type == expected; }

static inline bool lexer_next_is_and_not_eof(Lexer *lexer, Token_Type expected) {
  Token tok = lexer_peek(lexer);
  return tok.type == expected && tok.type != TOKEN_EOF;
}

static inline Token_Type lexer_next(Lexer *lexer) { return lexer_peek(lexer).type; }

static inline Span lexer_span(Lexer *lexer) { return lexer_peek(lexer).span; }

static inline Token lexer_lookahead(Lexer *lexer, size_t n_tokens) {
  lexer_fill_buffer(lexer);
  if (n_tokens == 0 || n_tokens > lexer->lookahead.length)
    return (Token){.type = TOKEN_EOF, .span = lexer->lookahead.data[0].span};
  return lexer->lookahead.data[n_tokens];
}

static inline bool token_is_operator(Token_Type type) {
  switch (type) {
  case TOKEN_LPAREN:
  case TOKEN_PLUS:
  case TOKEN_MINUS:
  case TOKEN_STAR:
  case TOKEN_SLASH:
  case TOKEN_XOR:
  case TOKEN_ASSIGN:
  case TOKEN_LOGICAL_AND:
  case TOKEN_LOGICAL_OR:
  case TOKEN_LOGICAL_NOT:
  case TOKEN_EQUALS:
  case TOKEN_NOT_EQUALS:
  case TOKEN_GREATER:
  case TOKEN_LESS:
  case TOKEN_BIT_AND:
  case TOKEN_BIT_OR:
  case TOKEN_BIT_NOT:
  case TOKEN_SHIFT_LEFT:
  case TOKEN_SHIFT_RIGHT:
    return true;
  default:
    return false;
  }
}

static inline bool lexer_next_is_operator(Lexer *lexer) {
  Token_Type type = lexer_next(lexer);
  return token_is_operator(type);
}

static inline void lexer_free(Lexer *lexer) {
  free((char *)lexer->input);
  LIST_FREE(lexer->lookahead);
}

#endif