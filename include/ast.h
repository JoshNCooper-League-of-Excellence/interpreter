#ifndef AST_H
#define AST_H

#include "binding.h"
#include "lexer.h"
#include "list.h"
#include "type.h"
#include <stddef.h>

typedef enum {
  PREC_NONE = 0,
  PREC_ASSIGNMENT,  // =
  PREC_LOGICAL_OR,  // ||
  PREC_LOGICAL_AND, // &&
  PREC_XOR,         // |
  PREC_BIT_OR,      // |
  PREC_BIT_AND,     // &
  PREC_EQUALITY,    // == !=
  PREC_RELATIONAL,  // < > <= >=
  PREC_SHIFT,       // << >>
  PREC_TERM,        // + -
  PREC_FACTOR,      // * / %
  PREC_POSTFIX,
  PREC_UNARY,   // ! ~ - + ^
  PREC_PRIMARY, // literals, identifiers
} Precedence;

typedef enum {
  EXPR_UNARY,
  EXPR_BINARY,
  EXPR_POSTFIX,
} Expression_Type;

typedef enum {
  OPERATOR_NONE = 0,
  OPERATOR_MODULO,
  OPERATOR_DEREFERENCE,
  OPERATOR_ADDRESS_OF,
  OPERATOR_NEGATE,
  OPERATOR_ASSIGN,
  OPERATOR_LOGICAL_OR,
  OPERATOR_LOGICAL_AND,
  OPERATOR_BIT_OR,
  OPERATOR_BIT_AND,
  OPERATOR_XOR,
  OPERATOR_EQUALS,
  OPERATOR_NOT_EQUALS,
  OPERATOR_LESS,
  OPERATOR_GREATER,
  OPERATOR_LESS_EQUAL,
  OPERATOR_GREATER_EQUAL,
  OPERATOR_SHIFT_LEFT,
  OPERATOR_SHIFT_RIGHT,
  OPERATOR_ADD,
  OPERATOR_SUB,
  OPERATOR_MUL,
  OPERATOR_DIV,
  OPERATOR_LOGICAL_NOT,
  OPERATOR_BIT_NOT,
  OPERATOR_PLUS_ASSIGN,
  OPERATOR_MINUS_ASSIGN,
  OPERATOR_STAR_ASSIGN,
  OPERATOR_SLASH_ASSIGN,
  OPERATOR_BIT_OR_ASSIGN,
  OPERATOR_BIT_AND_ASSIGN,
  OPERATOR_SHIFT_LEFT_ASSIGN,
  OPERATOR_SHIFT_RIGHT_ASSIGN,
  OPERATOR_INDEX,
} Operator;

static inline const char *operator_to_string(Operator op) {
  switch (op) {
  case OPERATOR_NONE:
    return "none";
  case OPERATOR_DEREFERENCE:
    return "*";
  case OPERATOR_ADDRESS_OF:
    return "&";
  case OPERATOR_NEGATE:
    return "-";
  case OPERATOR_ASSIGN:
    return "=";
  case OPERATOR_LOGICAL_OR:
    return "||";
  case OPERATOR_LOGICAL_AND:
    return "&&";
  case OPERATOR_BIT_OR:
    return "|";
  case OPERATOR_BIT_AND:
    return "&";
  case OPERATOR_EQUALS:
    return "==";
  case OPERATOR_NOT_EQUALS:
    return "!=";
  case OPERATOR_LESS:
    return "<";
  case OPERATOR_GREATER:
    return ">";
  case OPERATOR_LESS_EQUAL:
    return "<=";
  case OPERATOR_GREATER_EQUAL:
    return ">=";
  case OPERATOR_SHIFT_LEFT:
    return "<<";
  case OPERATOR_SHIFT_RIGHT:
    return ">>";
  case OPERATOR_ADD:
    return "+";
  case OPERATOR_SUB:
    return "-";
  case OPERATOR_MUL:
    return "*";
  case OPERATOR_DIV:
    return "/";
  case OPERATOR_LOGICAL_NOT:
    return "!";
  case OPERATOR_BIT_NOT:
    return "~";
  case OPERATOR_PLUS_ASSIGN:
    return "+=";
  case OPERATOR_MINUS_ASSIGN:
    return "-=";
  case OPERATOR_STAR_ASSIGN:
    return "*=";
  case OPERATOR_SLASH_ASSIGN:
    return "/=";
  case OPERATOR_BIT_OR_ASSIGN:
    return "|=";
  case OPERATOR_BIT_AND_ASSIGN:
    return "&=";
  case OPERATOR_SHIFT_LEFT_ASSIGN:
    return "<<=";
  case OPERATOR_SHIFT_RIGHT_ASSIGN:
    return ">>=";
  case OPERATOR_INDEX:
    return "[]";
  case OPERATOR_XOR:
    return "^";
  case OPERATOR_MODULO:
    return "%";
    break;
  }
}

static inline bool operator_is_compound(Operator op) {
  switch (op) {
  case OPERATOR_PLUS_ASSIGN:
  case OPERATOR_MINUS_ASSIGN:
  case OPERATOR_STAR_ASSIGN:
  case OPERATOR_SLASH_ASSIGN:
  case OPERATOR_BIT_OR_ASSIGN:
  case OPERATOR_BIT_AND_ASSIGN:
  case OPERATOR_SHIFT_LEFT_ASSIGN:
  case OPERATOR_SHIFT_RIGHT_ASSIGN:
    return true;
  default:
    return false;
  }
}

typedef enum {
  AST_ERROR,
  AST_PROGRAM,
  AST_LITERAL,
  AST_IDENTIFIER,
  AST_FUNCTION,
  AST_UNARY,
  AST_BINARY,
  AST_AGGREGATE_INITIALIZER,
  AST_MEMBER_ACCESS,

  AST_BLOCK,
  AST_RETURN,
  AST_VARIABLE,
  AST_CALL,
  AST_EXTERN,
  AST_TYPE,
  AST_STRUCT,
  AST_IF,
  AST_WHILE,
  AST_FOR,
} Ast_Tag;

typedef struct {
  const char *name;
  struct Ast *type;
  bool nameless;
} Parameter;

DEFINE_LIST(Parameter);

typedef struct string_list {
  const char **data;
  unsigned int length;
  unsigned int capacity;
} string_list;

typedef struct {
  const char *name;
  struct Ast *type;
} Ast_Struct_Member;

DEFINE_LIST(Ast_Struct_Member)

typedef struct Ast {
  Binding *binding;
  Span span;
  Ast_Tag tag;
  size_t index;
  union {
    Ast_Ptr_list program;
    Ast_Ptr_list block;
    struct Ast *return_value;

    struct {
      struct Ast *init;
      struct Ast *condition;
      struct Ast *update;
      struct Ast *block;
    } $for;

    struct {
      const char *name;
      Ast_Struct_Member_list members;
    } $struct;

    struct {
      struct Ast *condition;
      struct Ast *then_block;
      struct Ast *else_block; // optional
    } $if;

    struct {
      struct Ast *condition; // optional
      struct Ast *block;
    } $while;

    struct {
      struct Ast *base;
      const char *member;
    } member_access;

    struct {
      string_list keys;
      Ast_Ptr_list values;
      struct Ast *annotated_type;
    } aggregate_initializer;

    struct {
      const char *path;
      Type_Extension_list extensions;
    } type;

    struct {
      const char *name;
      struct Ast *type;
      struct Ast *initializer;
    } variable;

    struct {
      struct Ast *left, *right;
      Operator op;
    } binary;

    struct {
      struct Ast *operand;
      Operator op;
    } unary;

    const char *identifier;
    struct {
      const char *value;
      enum {
        AST_LITERAL_STRING,
        AST_LITERAL_INTEGER,
        AST_LITERAL_BOOL,
      } tag;
    } literal;

    struct {
      struct Ast *block;
      Parameter_list parameters;
      const char *name;
      struct Ast *return_type;
    } function;

    struct {
      Parameter_list parameters;
      struct Ast *return_type;
      const char *name;
    } extern_function;

    struct {
      const char *callee;
      Ast_Ptr_list arguments;
    } call;

    struct {
      const char *message;
    } error;
  };

} Ast;


// used to maintain homogenous aggregate initializers, only key values, or only
// values. no mixing the kinds
#define AGGREGATE_INITIALIZER_SET_CHECK_PARSING_KEY_VALUES(parsing_flag)                                                    \
  do {                                                                                                                      \
    if (!set) {                                                                                                             \
      parsing_only_values = (parsing_flag);                                                                                 \
      set = true;                                                                                                           \
    } else if ((parsing_only_values) != (parsing_flag)) {                                                                   \
      return parser_error(context, span,                                                                                    \
                          "aggregate initializers can only contain all "                                                    \
                          "values, or all key-value pairs.");                                                               \
    }                                                                                                                       \
  } while (0)

#endif