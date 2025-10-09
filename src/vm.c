#include "vm.h"
#include "list.h"
#include "string_builder.h"
#include "tac.h"
#include "type.h"
#include <ffi.h>

#define VM_ERRF(msg, ...)                                                      \
  fprintf(stderr, "[VM]: " msg "\n" __VA_OPT__(, ) __VA_ARGS__);               \
  exit(1);

Stack_Frame enter(Function *fn, int ret_dest, int caller) {
  Stack_Frame frame;
  frame.fn = fn;
  // always allocate at least a byte, to simplify freeing for now

  if (fn->n_locals) {
    frame.locals = calloc(fn->n_locals, sizeof(Value));
  } else {
    frame.locals = nullptr;
  }
  frame.n_locals = fn->n_locals;
  frame.ip = 0;
  frame.ret_dest = ret_dest;
  frame.caller = caller;
  return frame;
}

char *fix_escape_characters(const char *s) {
  size_t len = strlen(s);
  char *result = malloc(len + 1);
  if (!result)
    return NULL;

  size_t j = 0;
  for (size_t i = 0; i < len; ++i) {
    if (s[i] == '\\' && i + 1 < len) {
      switch (s[i + 1]) {
      case 'n':
        result[j++] = '\n';
        ++i;
        break;
      case 't':
        result[j++] = '\t';
        ++i;
        break;
      case 'r':
        result[j++] = '\r';
        ++i;
        break;
      case '\\':
        result[j++] = '\\';
        ++i;
        break;
      case '"':
        result[j++] = '"';
        ++i;
        break;
      case '\'':
        result[j++] = '\'';
        ++i;
        break;
      case '0':
        result[j++] = '\0';
        ++i;
        break;
      default:
        result[j++] = s[i];
        break;
      }
    } else {
      result[j++] = s[i];
    }
  }
  result[j] = '\0';
  return result;
}

void vm_execute(Module *m) {
  if (m->functions.length == 0) {
    VM_ERRF("module had no function, so we have nothing to execute.");
    return;
  }
  if (!m->entry_point) {
    VM_ERRF("undefined reference to 'main'");
    return;
  }

  Value constants[1024] = {0};

  // First, we process all constants into a buffer of 'Value's
  LIST_FOREACH(m->constants, constant) {
    if (constant.type == CONST_INTEGER) {
      constants[__i] =
          (Value){.integer = atoi(constant.value), .type = VALUE_INTEGER};
    } else if (constant.type == CONST_STRING) {

      constants[__i] = (Value){.string = fix_escape_characters(constant.value),
                               .type = VALUE_STRING};
    }
  }

  // execution context.
  Function *f = m->entry_point;

  Stack_Frame call_stack[256];
  int sp = 0;

  Value arg_stack[64];
  int arg_count = 0;

  Function *entry = m->entry_point;
  call_stack[0] = enter(entry, -1, -1);
  sp = 0;

  while (sp >= 0) {
    Stack_Frame *sf = &call_stack[sp];
    if (sf->ip >= sf->fn->code.length) {
      if (sf->caller == -1) {
        break;
      }
      Value rv = sf->locals[0];

      leave(sf);

      int caller_idx = sf->caller;
      int dest = sf->ret_dest;
      sp = caller_idx;
      if (dest >= 0)
        call_stack[sp].locals[dest] = rv;
      continue;
    }

    Instr instr = sf->fn->code.data[sf->ip++];

    switch (instr.op) {
    case OP_ALLOCA: {
      int dest = instr.a;
      int idx = instr.b;
      if (idx < 0 || (unsigned)idx >= m->types.length) {
        VM_ERRF("invalid type index %d", idx);
      }
      call_stack[sp].locals[dest] = default_value_of_type(m->types.data[idx]);
    } break;

    case OP_MEMBER_LOAD: {
      int dest = instr.a;
      int struct_slot = instr.b;
      int member_idx = instr.c;
      Value *struct_val = &call_stack[sp].locals[struct_slot];
      if (struct_val->type != VALUE_STRUCT) {
        VM_ERRF("OP_MEMBER_LOAD: value at slot %d is not a struct",
                struct_slot);
      }
      if (member_idx < 0 || (size_t)member_idx >= struct_val->$struct.length) {
        VM_ERRF("OP_MEMBER_LOAD: invalid member index %d", member_idx);
      }
      call_stack[sp].locals[dest] = struct_val->$struct.members[member_idx];
    } break;
    case OP_MEMBER_STORE: {
      int struct_slot = instr.a;
      int member_idx = instr.b;
      int src = instr.c;
      Value *struct_val = &call_stack[sp].locals[struct_slot];
      if (struct_val->type != VALUE_STRUCT) {
        VM_ERRF("OP_MEMBER_STORE: value at slot %d is not a struct",
                struct_slot);
      }
      if (member_idx < 0 || (size_t)member_idx >= struct_val->$struct.length) {
        VM_ERRF("OP_MEMBER_STORE: invalid member index %d", member_idx);
      }
      struct_val->$struct.members[member_idx] = call_stack[sp].locals[src];
    } break;

    case OP_CONST: {
      int dest = instr.a;
      int cidx = instr.b;
      call_stack[sp].locals[dest] = constants[cidx];
    } break;
    case OP_LOAD: {
      int dest = instr.a;
      int slot = instr.b;
      call_stack[sp].locals[dest] = call_stack[sp].locals[slot];
    } break;
    case OP_STORE: {
      int slot = instr.a;
      int src = instr.b;
      call_stack[sp].locals[slot] = call_stack[sp].locals[src];
    } break;
    case OP_ADD:
    case OP_SUB:
    case OP_MUL:
    case OP_DIV: {
      int dest = instr.a, l = instr.b, r = instr.c;
      int lv = call_stack[sp].locals[l].integer;
      int rv = call_stack[sp].locals[r].integer;
      int result = 0;
      if (instr.op == OP_ADD) {
        result = lv + rv;
      } else if (instr.op == OP_SUB) {
        result = lv - rv;
      } else if (instr.op == OP_MUL) {
        result = lv * rv;
      } else if (instr.op == OP_DIV) {
        result = lv / rv;
      }
      call_stack[sp].locals[dest].integer = result;
      call_stack[sp].locals[dest].type = VALUE_INTEGER;
    } break;
    case OP_ARG: {
      int index = instr.a;
      int src = instr.b;
      if (index < sizeof(arg_stack) / sizeof(Value)) {
        arg_stack[index] = call_stack[sp].locals[src];
        if (index + 1 > (size_t)arg_count) {
          arg_count = index + 1;
        }
      }
    } break;
    case OP_CALL_EXTERN: {
      int dest = instr.a;
      int func_idx = instr.b;
      int nargs = instr.c;

      if (func_idx < 0 || (unsigned)func_idx >= CACHED_EXTERNS.length) {
        VM_ERRF("invalid extern index %d", func_idx);
      }

      Extern_Function callee = CACHED_EXTERNS.data[func_idx];

      Value value = libffi_dynamic_dispatch(callee, arg_stack, arg_count);
      arg_count = 0;

      call_stack[sp].locals[dest] = value;

    } break;
    case OP_CALL: {
      int dest = instr.a;
      int func_idx = instr.b;
      int nargs = instr.c;

      if (func_idx < 0 || (unsigned)func_idx >= m->functions.length) {
        VM_ERRF("invalid function index %d", func_idx);
      }

      Function *callee = m->functions.data[func_idx];

      if (sp + 1 >= (int)(sizeof(call_stack) / sizeof(call_stack[0]))) {
        VM_ERRF("call stack overflow");
        return;
      }

      /* create callee frame */
      int new_sp = sp + 1;
      call_stack[new_sp] = enter(callee, dest, sp);

      // copy args, then clear the stack
      for (int i = 0; i < nargs && (size_t)i < callee->param_count; ++i) {
        call_stack[new_sp].locals[i] = arg_stack[i];
      }
      arg_count = 0;

      // jump to new stack frame
      sp = new_sp;
    } break;
    case OP_RET: {
      int src = instr.a;
      Value rv = {.type = VALUE_VOID};
      if (src >= 0) {
        rv = call_stack[sp].locals[src];
      }
      int caller_idx = call_stack[sp].caller;
      int ret_dest = call_stack[sp].ret_dest;

      leave(sf);

      if (caller_idx == -1) {
        return;
      }
      sp = caller_idx;
      if (ret_dest >= 0)
        call_stack[sp].locals[ret_dest] = rv;
    } break;

    default:
      VM_ERRF("unknown opcode %d", instr.op);
      return;
    }
  }

  leave(&call_stack[0]);
}

void print_value(Value *value, String_Builder *sb) {
  switch (value->type) {
  case VALUE_INTEGER:
    sb_appendf(sb, "%d", value->integer);
    break;
  case VALUE_STRING:
    sb_appendf(sb, "\"%s\"", value->string ? value->string : "(null)");
    break;
  case VALUE_STRUCT: {
    sb_appendf(sb, "{");
    for (size_t i = 0; i < value->$struct.length; ++i) {
      print_value(&value->$struct.members[i], sb);
      if (i + 1 < value->$struct.length)
        sb_appendf(sb, ", ");
    }
    sb_appendf(sb, "}");
    break;
  }
  default:
    sb_appendf(sb, "<unknown>");
    break;
  }
}
Value default_value_of_type(Type *type) {
  switch (type->tag) {
  case TYPE_INT: {
    return (Value){
        .type = VALUE_INTEGER,
        .integer = 0,
    };
  }
  case TYPE_STRING: {
    return (Value){
        .type = VALUE_STRING,
        .string = nullptr,
    };
  }
  case TYPE_STRUCT: {
    Struct_Type *struct_type = (Struct_Type *)type;
    Value v;
    v.type = VALUE_STRUCT;
    v.$struct.length = struct_type->members.length;
    v.$struct.members = calloc(struct_type->members.length, sizeof(Value));
    for (size_t i = 0; i < struct_type->members.length; ++i) {
      v.$struct.members[i] =
          default_value_of_type(struct_type->members.data[i].type);
    }
    return v;
  }
  case TYPE_FUNCTION:
  case TYPE_VOID:
    return (Value){.type = VALUE_VOID};
    break;
  }
}

void value_free(Value *value) {
  if (value->type == VALUE_STRUCT) {
    for (int i = 0; i < value->$struct.length; ++i) {
      value_free(&value->$struct.members[i]);
    }
    free(value->$struct.members);
  }
  *value = (Value){0};
}

void leave(Stack_Frame *frame) {
  if (!frame->locals) {
    return;
  }

  for (int i = 0; i < frame->n_locals; ++i) {
    Value *value = &frame->locals[i];
    value_free(value);
  }
}
