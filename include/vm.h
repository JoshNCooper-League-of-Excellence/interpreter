#ifndef VM_H
#define VM_H
#include "tac.h"

typedef enum {
  VALUE_INTEGER,
  VALUE_STRING,
} Value_Type;

typedef struct Value {
  Value_Type type;
  union {
    int integer;
    char *string;
  };
} Value;

#define VM_STACK_LENGTH 1024

typedef struct {
  Function *fn;
  Value *locals; /* length = fn->n_locals */
  unsigned ip;
  int ret_dest; /* caller temp index to store return, -1 = ignore */
  int caller;   /* index of caller frame in call_stack, -1 = none */
} Stack_Frame;

void vm_execute(Module *module);

#endif // #ifndef VM_H