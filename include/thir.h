#ifndef THIR_H
#define THIR_H

#include "ast.h"
#include "binding.h"
#include "lexer.h"
#include "type.h"

typedef enum {
  THIR_PROGRAM,
  THIR_VARIABLE,
  THIR_FUNCTION,
  THIR_BLOCK,
  THIR_LITERAL,
  THIR_UNARY,
  THIR_BINARY,
  THIR_RETURN,
  THIR_CALL,
  THIR_EXTERN,
  THIR_AGGREGATE_INITIALIZER,
  THIR_ARRAY_INITIALIZER,
  THIR_MEMBER_ACCESS,
  THIR_IF,
  THIR_LOOP,
} Thir_Tag;

#include "ffi.h"

DEFINE_LIST(ffi_type);

typedef struct {
  const char *name;
  ffi_type_list parameters;
  ffi_type return_type;
  size_t index;
  void *ptr;
  Type *original_return_type;
  Function_Type *function_type;
} Extern_Function;

struct Thir;
DEFINE_LIST(Extern_Function);
extern Extern_Function_list CACHED_EXTERNS;
Extern_Function get_ffi_function_from_thir(struct Thir *thir);

typedef struct Thir {
  Binding *binding;
  Thir_Tag tag;
  Span span;
  size_t index;
  Type *type;
  union {
    Thir_Ptr_list program;
    Thir_Ptr_list block;

    // Both while and for use this.
    // while just doesn't use the initializer nor increment
    struct {
      struct Thir *initializer; // optional.
      struct Thir *condition;
      struct Thir *increment; // optional
      struct Thir *block;
    } loop;

    struct {
      struct Thir *condition;
      struct Thir *then_block;
      struct Thir *else_block;
    } $if;

    struct {
      Thir_Ptr_list values;
      string_list keys;
    } aggregate_initializer;

    struct {
      Thir_Ptr_list values;
    } array_initializer;

    struct {
      struct Thir *base;
      unsigned index;
    } member_access;

    struct {
      Type *return_type;
      Binding_Ptr_list parameters;
      const char *name;
      size_t index; // index of this function in CACHED_EXTERNS
    } extern_function;

    struct {
      Type *return_type;
      Binding_Ptr_list parameters;
      struct Thir *block;
      const char *name;
    } function;

    struct {
      Binding *callee;
      Thir_Ptr_list arguments;
      Type *type;
    } call;

    struct {
      const char *value;
    } literal;

    struct {
      struct Thir *left, *right;
      Operator op;
    } binary;

    struct {
      struct Thir *operand;
      Operator op;
    } unary;

    struct Thir *variable_initializer;
    struct Thir *return_value;
  };
} Thir;

Thir *type_program(struct Ast *, Context *context);
Thir *type_literal(struct Ast *, Context *context);
Thir *type_call(struct Ast *, Context *context);
Thir *type_expression(struct Ast *, Context *context);
Thir *type_identifier(struct Ast *, Context *context);
Thir *type_block(struct Ast *, Context *context);
Thir *type_function(struct Ast *, Context *context);
Thir *type_extern(struct Ast *, Context *context);
void type_struct(struct Ast *, Context *context);
Thir *type_unary(struct Ast *, Context *context);
Thir *type_binary(struct Ast *, Context *context);
Thir *type_aggregate_initializer(struct Ast *, Context *context);
Thir *type_return(struct Ast *, Context *context);
Thir *type_variable(struct Ast *, Context *context);
Type *get_type_from_ast_type(struct Ast *, Context *context);


#endif