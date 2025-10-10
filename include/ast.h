#ifndef AST_H
#define AST_H

#include "binding.h"
#include "lexer.h"
#include "list.h"
#include <stddef.h>

typedef enum {
  AST_ERROR,
  AST_PROGRAM,
  AST_LITERAL,
  AST_IDENTIFIER,
  AST_BLOCK,
  AST_FUNCTION,
  AST_UNARY,
  AST_BINARY,
  AST_RETURN,
  AST_VARIABLE,
  AST_CALL,
  AST_EXTERN,
  AST_TYPE,
  AST_AGGREGATE_INITIALIZER,
  AST_STRUCT,
  AST_MEMBER_ACCESS,
} Ast_Tag;

typedef struct {
  const char *identifier;
  const char *type;
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
      const char *name;
      Ast_Struct_Member_list members;
    } $struct;

    struct {
      struct Ast *base;
      const char *member;
    } member_access;

    struct {
      string_list keys;
      Ast_Ptr_list values;
    } aggregate_initializer;

    struct {
      const char *path;
      // Type_Extension_list extensions;
    } type;

    struct {
      const char *name;
      struct Ast *type;
      struct Ast *initializer;
    } variable;

    struct {
      struct Ast *left, *right;
      Token_Type op;
    } binary;

    struct {
      struct Ast *operand;
      Token_Type op;
    } unary;

    const char *identifier;
    struct {
      const char *value;
      enum {
        STRING,
        INTEGER,
      } tag;
    } literal;

    struct {
      struct Ast *block;
      Parameter_list parameters;
      const char *return_type;
      const char *name;
    } function;

    struct {
      Parameter_list parameters;
      const char *return_type;
      const char *name;
    } extern_function;

    struct {
      const char *callee;
      Ast_Ptr_list arguments;
    } call;

    struct {
      const char *message;
      bool fatal;
    } error;
  };

} Ast;

#include "string_builder.h"

static inline void print_ast(Ast *ast, String_Builder *sb);

static inline void print_indent(String_Builder *sb, int indent) {
  for (int i = 0; i < indent; ++i) {
    sb_append(sb, "  ");
  }
}

static const char *ast_tag_names[] = {
    "ERROR", "PROGRAM", "LITERAL", "IDENTIFIER", "BLOCK", "FUNCTION",
    "UNARY", "BINARY",  "RETURN",  "VARIABLE",   "CALL",  "EXTERN"};

static inline void print_ast_rec(Ast *node, String_Builder *sb, int indent) {
  if (!node) {
    print_indent(sb, indent);
    sb_append(sb, "null\n");
    return;
  }
  print_indent(sb, indent);
  sb_append(sb, ast_tag_names[node->tag]);
  sb_append(sb, "\n");

  switch (node->tag) {
  case AST_MEMBER_ACCESS:
    print_indent(sb, indent + 1);
    sb_append(sb, "base:\n");
    print_ast_rec(node->member_access.base, sb, indent + 2);
    print_indent(sb, indent + 1);
    sb_append(sb, "member: ");
    sb_append(sb, node->member_access.member);
    sb_append(sb, "\n");
    break;
  case AST_STRUCT:
    print_indent(sb, indent + 1);
    sb_append(sb, "members:\n");
    for (size_t i = 0; i < node->$struct.members.length; ++i) {
      Ast_Struct_Member *member = &node->$struct.members.data[i];
      print_indent(sb, indent + 2);
      sb_append(sb, "name: ");
      sb_append(sb, member->name);
      sb_append(sb, "\n");
      print_indent(sb, indent + 2);
      sb_append(sb, "type:\n");
      print_ast_rec(member->type, sb, indent + 3);
    }
    break;

  case AST_AGGREGATE_INITIALIZER:
    print_indent(sb, indent + 1);
    sb_append(sb, "values:\n");
    for (size_t i = 0; i < node->aggregate_initializer.values.length; ++i) {
      print_indent(sb, indent + 2);
      if (node->aggregate_initializer.keys.data &&
          i < node->aggregate_initializer.keys.length &&
          node->aggregate_initializer.keys.data[i]) {
        sb_append(sb, "key: ");
        sb_append(sb, node->aggregate_initializer.keys.data[i]);
        sb_append(sb, "\n");
        print_indent(sb, indent + 2);
      }
      sb_append(sb, "value:\n");
      print_ast_rec(node->aggregate_initializer.values.data[i], sb, indent + 3);
    }
    break;
  case AST_TYPE:
    print_indent(sb, indent + 1);
    sb_append(sb, "path: ");
    sb_append(sb, node->type.path ? node->type.path : "null");
    sb_append(sb, "\n");
    break;
  case AST_EXTERN: {
    print_indent(sb, indent + 1);
    sb_append(sb, "name: ");
    sb_append(sb, node->extern_function.name);
    sb_append(sb, "\n");
    print_indent(sb, indent + 1);
    sb_append(sb, "return_type: ");
    sb_append(sb, node->extern_function.return_type
                      ? node->extern_function.return_type
                      : "void");
    sb_append(sb, "\n");
    print_indent(sb, indent + 1);
    sb_append(sb, "parameters:\n");
    for (size_t i = 0; i < node->extern_function.parameters.length; ++i) {
      Parameter *param = &node->extern_function.parameters.data[i];
      print_indent(sb, indent + 2);
      sb_append(sb, param->type);
      sb_append(sb, " ");
      sb_append(sb, param->identifier);
      sb_append(sb, "\n");
    }
    break;
  }
  case AST_PROGRAM:
    for (size_t i = 0; i < node->program.length; ++i)
      print_ast_rec(node->program.data[i], sb, indent + 1);
    break;
  case AST_BLOCK:
    for (size_t i = 0; i < node->block.length; ++i)
      print_ast_rec(node->block.data[i], sb, indent + 1);
    break;
  case AST_LITERAL:
    print_indent(sb, indent + 1);
    sb_append(sb, "value: ");
    sb_append(sb, node->literal.value);
    sb_append(sb, "\n");
    print_indent(sb, indent + 1);
    sb_append(sb, "type: ");
    sb_append(sb, node->literal.tag == STRING ? "STRING" : "INTEGER");
    sb_append(sb, "\n");
    break;
  case AST_IDENTIFIER:
    print_indent(sb, indent + 1);
    sb_append(sb, "name: ");
    sb_append(sb, node->identifier);
    sb_append(sb, "\n");
    break;
  case AST_FUNCTION:
    print_indent(sb, indent + 1);
    sb_append(sb, "name: ");
    sb_append(sb, node->function.name);
    sb_append(sb, "\n");
    print_indent(sb, indent + 1);
    sb_append(sb, "return_type: ");
    sb_append(sb,
              node->function.return_type ? node->function.return_type : "void");
    sb_append(sb, "\n");
    print_indent(sb, indent + 1);
    sb_append(sb, "parameters:\n");
    for (size_t i = 0; i < node->function.parameters.length; ++i) {
      Parameter *param = &node->function.parameters.data[i];
      print_indent(sb, indent + 2);
      sb_append(sb, param->type);
      sb_append(sb, " ");
      sb_append(sb, param->identifier);
      sb_append(sb, "\n");
    }
    print_indent(sb, indent + 1);
    sb_append(sb, "body:\n");
    print_ast_rec(node->function.block, sb, indent + 2);
    break;
  case AST_UNARY:
    print_indent(sb, indent + 1);
    sb_append(sb, "op: ");
    sb_append(sb, token_type_to_string(node->unary.op));
    sb_append(sb, "\n");
    print_indent(sb, indent + 1);
    sb_append(sb, "operand:\n");
    print_ast_rec(node->unary.operand, sb, indent + 2);
    break;
  case AST_BINARY:
    print_indent(sb, indent + 1);
    sb_append(sb, "op: ");
    sb_append(sb, token_type_to_string(node->binary.op));
    sb_append(sb, "\n");
    print_indent(sb, indent + 1);
    sb_append(sb, "left:\n");
    print_ast_rec(node->binary.left, sb, indent + 2);
    print_indent(sb, indent + 1);
    sb_append(sb, "right:\n");
    print_ast_rec(node->binary.right, sb, indent + 2);
    break;
  case AST_RETURN:
    print_indent(sb, indent + 1);
    sb_append(sb, "value:\n");
    print_ast_rec(node->return_value, sb, indent + 2);
    break;
  case AST_VARIABLE:
    print_indent(sb, indent + 1);
    sb_append(sb, "name: ");
    sb_append(sb, node->variable.name);
    sb_append(sb, "\n");
    print_indent(sb, indent + 1);
    sb_append(sb, "initializer:\n");
    print_ast_rec(node->variable.initializer, sb, indent + 2);
    break;
  case AST_CALL:
    print_indent(sb, indent + 1);
    sb_append(sb, "callee: ");
    sb_append(sb, node->call.callee);
    sb_append(sb, "\n");
    print_indent(sb, indent + 1);
    sb_append(sb, "arguments:\n");
    for (size_t i = 0; i < node->call.arguments.length; ++i)
      print_ast_rec(node->call.arguments.data[i], sb, indent + 2);
    break;
  case AST_ERROR:
    print_indent(sb, indent + 1);
    sb_append(sb, "message: ");
    sb_append(sb, node->error.message);
    sb_append(sb, "\n");
    print_indent(sb, indent + 1);
    sb_append(sb, "fatal: ");
    sb_append(sb, node->error.fatal ? "true" : "false");
    sb_append(sb, "\n");
    break;
  }
}

static inline void print_ast(Ast *ast, String_Builder *sb) {
  print_ast_rec(ast, sb, 0);
}

// used to maintain homogenous aggregate initializers, only key values, or only
// values. no mixing the kinds
#define AGGREGATE_INITIALIZER_SET_CHECK_PARSING_KEY_VALUES(parsing_flag)       \
  do {                                                                         \
    if (!set) {                                                                \
      parsing_only_values = (parsing_flag);                                    \
      set = true;                                                              \
    } else if ((parsing_only_values) != (parsing_flag)) {                      \
      return parser_error(context, span,                                       \
                          "aggregate initializers can only contain all "       \
                          "values, or all key-value pairs.",                   \
                          true);                                               \
    }                                                                          \
  } while (0)

#endif