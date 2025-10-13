#ifndef VM_H
#define VM_H

#include "string_builder.h"
#include "ir.h"

typedef struct {
  int argc;
  char **argv;
} Cmd_Line_Args;

extern Cmd_Line_Args COMMAND_LINE_ARGUMENTS;

typedef enum {
  VALUE_VOID, // this exists just for foreign functions that return void.
  VALUE_INTEGER,
  VALUE_POINTER,
  VALUE_STRUCT,
} Value_Type;

typedef struct Value {
  unsigned owner_uid;
  Value_Type tag;
  Type *type;
  union {
    signed long long integer;
    struct {
      struct Value *members;
      // this is num members
      unsigned length;
    } $struct;
    struct { // this is used for both pointers and arrays.
      void *elements;
      enum {
        POINTEE_VALUE,
        POINTEE_RAW,
      } pointee;
      unsigned length;
    } pointer;
  };
} Value;

void vfree(Value *value, unsigned owner_uid);
Value vcopy(Value value);

#define VM_STACK_LENGTH 1024

typedef struct {
  Function *fn;
  Value *locals; /* length = fn->n_locals */
  int n_locals;
  unsigned ip;
  int ret_dest; /* caller temp index to store return, -1 = ignore */
  int caller;   /* index of caller frame in call_stack, -1 = none */
  unsigned uid;
} Stack_Frame;

void vm_execute(Module *module);

Value libffi_dynamic_dispatch(Extern_Function function, Value *argv, int argc);

Value default_array_of_type(Type *type, unsigned owner_uid, unsigned long long length);
Value default_value_of_type(Type *type, unsigned owner_id);

void print_value(Value *value, String_Builder *sb);

void leave(Stack_Frame *frame);
#endif // #ifndef VM_H
