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
  THIR_CONTROL_FLOW_CHANGE,
  THIR_LABEL,
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

    // we don't really need to store anything here?
    // we'll end up tracking it and 'linking' during/after IR gen.
    struct {
      const char *name;
    } label;

    struct {
      Control_Flow_Tag tag;
      // uniquely this is not resolved by the typer but actually the IR gen.
      const char *target_label; 
    } control_flow_change;

    // Both while and for use this.
    // while just doesn't use the initializer nor increment
    struct {
      struct Thir *init; // optional.
      struct Thir *condition;
      struct Thir *update; // optional
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

const char *get_function_type_string(Type_Ptr_list arguments, Type *return_type);

Thir *type_program(struct Ast *, Context *context);
Thir *type_literal(struct Ast *, Context *context);
Thir *type_call(struct Ast *, Context *context);
Thir *type_expression(struct Ast *, Context *context);
Thir *type_identifier(struct Ast *, Context *context);
Thir *type_block(struct Ast *, Context *context);
Thir *type_function(struct Ast *, Context *context);
Thir *type_extern(struct Ast *, Context *context);
Thir *type_unary(struct Ast *, Context *context);
Thir *type_binary(struct Ast *, Context *context);
Thir *type_aggregate_initializer(struct Ast *, Context *context);
Thir *type_return(struct Ast *, Context *context);
Thir *type_variable(struct Ast *, Context *context);
Type *get_type_from_ast_type(struct Ast *, Context *context);

[[noreturn]]
static inline void report_error(Ast *error) {
  if (error && error->tag == AST_ERROR) {
    const char *source_range = lexer_span_to_string(error->span);
    fprintf(stderr, "Error at span %s: '%s'\n", source_range, error->error.message ? error->error.message : "Unknown error");
  }
  exit(1);
}

[[noreturn]]
static inline void use_of_undeclared(const char *kind, const char *identifier, Span span) {
  char *span_string = lexer_span_to_string(span);
  fprintf(stderr, "use of undeclared %s '%s' at: %s", kind, identifier, span_string);
  exit(1);
}

Binding_Ptr_list convert_ast_parameters_to_thir_parameters(Context *context, Parameter_list parameters, Span span,
                                                           Type_Ptr_list *argument_types);

Type_Ptr_list collect_parameter_types(Parameter_list params, Span span, Context *ctx);

#endif