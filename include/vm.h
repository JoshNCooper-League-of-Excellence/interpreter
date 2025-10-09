#ifndef VM_H
#define VM_H

#include "string_builder.h"
#include "tac.h"

typedef struct {
  int argc;
  char **argv;
} Cmd_Line_Args;

extern Cmd_Line_Args COMMAND_LINE_ARGUMENTS;

typedef enum {
  VALUE_VOID, // this exists just for foreign functions that return void.
  VALUE_INTEGER,
  VALUE_STRING,
  VALUE_STRUCT,
} Value_Type;

typedef struct Value {
  Value_Type type;
  union {
    int integer;
    char *string;
    struct Value *$struct;
  };
} Value;

void print_value(Value *value, String_Builder *sb);

#define VM_STACK_LENGTH 1024

typedef struct {
  Function *fn;
  Value *locals; /* length = fn->n_locals */
  unsigned ip;
  int ret_dest; /* caller temp index to store return, -1 = ignore */
  int caller;   /* index of caller frame in call_stack, -1 = none */
} Stack_Frame;

void vm_execute(Module *module);

Value libffi_dynamic_dispatch(Extern_Function function, Value *argv, int argc);

#endif // #ifndef VM_H
