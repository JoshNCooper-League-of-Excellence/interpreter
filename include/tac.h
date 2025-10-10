#ifndef TAC_H
#define TAC_H

#include "list.h"
#include "thir.h"
#include "type.h"
#include <stdlib.h>

typedef enum {
  OP_CONST,
  OP_LOAD,
  OP_STORE,
  OP_CALL,
  OP_CALL_EXTERN,
  OP_RET,
  OP_PUSH,
  OP_ALLOCA,

  OP_ADD,
  OP_SUB,
  OP_MUL,
  OP_DIV,
  OP_MEMBER_LOAD,
  OP_MEMBER_STORE,
  OP_NEGATE,
  OP_LOGICAL_OR,
  OP_LOGICAL_AND,
  OP_SHIFT_LEFT,
  OP_SHIFT_RIGHT,
  OP_XOR,
  OP_BIT_OR,
  OP_BIT_AND,
  OP_EQUALS,
  OP_NOT_EQUALS,
  OP_LESS,
  OP_GREATER,
  OP_LOGICAL_NOT,
  OP_BIT_NOT,
  OP_MODULO,

  OP_JUMP_IF,
  OP_JUMP,

} Op_Code;

typedef struct {
  Op_Code op;
  signed long long a, b, c;
} Instr;

typedef struct {
  Instr *data;
  unsigned int length;
  unsigned int capacity;
} Instr_Buffer;

typedef enum {
  CONST_TYPE_STRING,
  CONST_TYPE_INT,
} Constant_Type;

static inline const char *constant_type_to_string(Constant_Type type) {
  switch (type) {
  case CONST_TYPE_STRING:
    return "byte*";
  case CONST_TYPE_INT:
    return "int";
  }
}

typedef struct {
  Constant_Type type;
  const char *value;
} Constant;

typedef struct {
  Constant *data;
  unsigned int length;
  unsigned int capacity;
} Constant_Buffer;

typedef struct {
  const char *name;
  Instr_Buffer code;
  unsigned n_locals;
  size_t param_count;
  size_t const_start;
} Function;

typedef struct {
  Function **data;
  unsigned int length;
  unsigned int capacity;
} Function_Buffer;

typedef struct {
  Function *entry_point;
  Function_Buffer functions;
  Constant_Buffer constants;
  Extern_Function_list externs;
  Type_Ptr_list types;
} Module;

void module_init(Module *m, Context *context);

#define EMIT($instruction_buffer, $instruction) LIST_PUSH($instruction_buffer, $instruction);

#define MAKE_INSTR0(op) ((Instr){(op), 0, 0, 0})
#define MAKE_INSTR1(op, a) ((Instr){(op), (a), 0, 0})
#define MAKE_INSTR2(op, a, b) ((Instr){(op), (a), (b), 0})
#define MAKE_INSTR3(op, a, b, c) ((Instr){(op), (a), (b), (c)})

#define EMIT_PUSH(buf, src) EMIT(buf, MAKE_INSTR1(OP_PUSH, (src)))
#define EMIT_CONST(buf, dest, const_idx) EMIT(buf, MAKE_INSTR2(OP_CONST, (dest), (const_idx)))
#define EMIT_LOAD(buf, dest, slot) EMIT(buf, MAKE_INSTR2(OP_LOAD, (dest), (slot)))
#define EMIT_STORE(buf, slot, src) EMIT(buf, MAKE_INSTR2(OP_STORE, (slot), (src)))

#define EMIT_ADD(buf, dest, l, r) EMIT(buf, MAKE_INSTR3(OP_ADD, (dest), (l), (r)))

#define EMIT_MODULO(buf, dest, l, r) EMIT(buf, MAKE_INSTR3(OP_MODULO, (dest), (l), (r)))

#define EMIT_SUB(buf, dest, l, r) EMIT(buf, MAKE_INSTR3(OP_SUB, (dest), (l), (r)))

#define EMIT_MUL(buf, dest, l, r) EMIT(buf, MAKE_INSTR3(OP_MUL, (dest), (l), (r)))

#define EMIT_DIV(buf, dest, l, r) EMIT(buf, MAKE_INSTR3(OP_DIV, (dest), (l), (r)))

#define EMIT_CALL(buf, dest, func, narg) EMIT(buf, MAKE_INSTR3(OP_CALL, (dest), (func), (narg)))

#define EMIT_CALL_EXTERN(buf, dest, index, nargs) EMIT(buf, MAKE_INSTR3(OP_CALL_EXTERN, (dest), (index), (nargs)))

#define EMIT_RET(buf, src) EMIT(buf, MAKE_INSTR1(OP_RET, (src)))

#define EMIT_MEMBER_LOAD(buf, dest, target, index) EMIT(buf, MAKE_INSTR3(OP_MEMBER_LOAD, (dest), (target), (index)))

#define EMIT_MEMBER_STORE(buf, target, index, src) EMIT(buf, MAKE_INSTR3(OP_MEMBER_STORE, (target), (index), (src)))

#define EMIT_ALLOCA(buf, dest, type_index, length) EMIT(buf, MAKE_INSTR3(OP_ALLOCA, (dest), (type_index), (length)))

#define EMIT_DEREFERENCE(buf, dest, src) EMIT(buf, MAKE_INSTR2(OP_DEREFERENCE, (dest), (src)))

#define EMIT_ADDRESS_OF(buf, dest, src) EMIT(buf, MAKE_INSTR2(OP_ADDRESS_OF, (dest), (src)))

#define EMIT_NEGATE(buf, dest, src) EMIT(buf, MAKE_INSTR2(OP_NEGATE, (dest), (src)))

#define EMIT_ASSIGN(buf, dest, src) EMIT(buf, MAKE_INSTR2(OP_ASSIGN, (dest), (src)))

#define EMIT_LOGICAL_OR(buf, dest, l, r) EMIT(buf, MAKE_INSTR3(OP_LOGICAL_OR, (dest), (l), (r)))

#define EMIT_LOGICAL_AND(buf, dest, l, r) EMIT(buf, MAKE_INSTR3(OP_LOGICAL_AND, (dest), (l), (r)))

#define EMIT_BIT_OR(buf, dest, l, r) EMIT(buf, MAKE_INSTR3(OP_BIT_OR, (dest), (l), (r)))

#define EMIT_BIT_AND(buf, dest, l, r) EMIT(buf, MAKE_INSTR3(OP_BIT_AND, (dest), (l), (r)))

#define EMIT_EQUALS(buf, dest, l, r) EMIT(buf, MAKE_INSTR3(OP_EQUALS, (dest), (l), (r)))

#define EMIT_NOT_EQUALS(buf, dest, l, r) EMIT(buf, MAKE_INSTR3(OP_NOT_EQUALS, (dest), (l), (r)))

#define EMIT_LESS(buf, dest, l, r) EMIT(buf, MAKE_INSTR3(OP_LESS, (dest), (l), (r)))

#define EMIT_GREATER(buf, dest, l, r) EMIT(buf, MAKE_INSTR3(OP_GREATER, (dest), (l), (r)))

#define EMIT_MINUS(buf, dest, l, r) EMIT(buf, MAKE_INSTR3(OP_MINUS, (dest), (l), (r)))

#define EMIT_LOGICAL_NOT(buf, dest, src) EMIT(buf, MAKE_INSTR2(OP_LOGICAL_NOT, (dest), (src)))

#define EMIT_BIT_NOT(buf, dest, src) EMIT(buf, MAKE_INSTR2(OP_BIT_NOT, (dest), (src)))

#define EMIT_INDEX(buf, dest, base, index) EMIT(buf, MAKE_INSTR3(OP_INDEX, (dest), (base), (index)))

#define EMIT_SHIFT_LEFT(buf, dest, l, r) EMIT(buf, MAKE_INSTR3(OP_SHIFT_LEFT, (dest), (l), (r)))

#define EMIT_SHIFT_RIGHT(buf, dest, l, r) EMIT(buf, MAKE_INSTR3(OP_SHIFT_RIGHT, (dest), (l), (r)))

#define EMIT_XOR(buf, dest, l, r) EMIT(buf, MAKE_INSTR3(OP_XOR, (dest), (l), (r)))

#define EMIT_JUMP_IF(buf, cond, target) EMIT(buf, MAKE_INSTR2(OP_JUMP_IF, (cond), (target)))

#define EMIT_JUMP(buf, target) EMIT(buf, MAKE_INSTR1(OP_JUMP, (target)))

int lower_expression(Thir *n, Function *fn, Module *m);
void lower_block(Thir *block, Function *fn, Module *m);
void lower_program(Thir *program, Module *m);
void lower_function(Thir *node, Module *m);

#include "string_builder.h"

void print_module(Module *m, String_Builder *sb);

#endif