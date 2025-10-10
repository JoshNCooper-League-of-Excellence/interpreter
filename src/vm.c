#include "vm.h"
#include "list.h"
#include "string_builder.h"
#include "tac.h"
#include "type.h"
#include <dlfcn.h>
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
  }
  if (!m->entry_point) {
    VM_ERRF("undefined reference to 'main'");
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
  int arg_p = 0;
#define ENTRY_POINT_CALLER -1

  Function *entry = m->entry_point;
  call_stack[0] = enter(entry, ENTRY_POINT_CALLER, ENTRY_POINT_CALLER);

  while (sp >= 0) {
    Stack_Frame *sf = &call_stack[sp];
    if (sf->ip >= sf->fn->code.length) {
      if (sf->caller == ENTRY_POINT_CALLER) {
        break;
      }
      Value rv = sf->locals[0];
      leave(sf);
      int caller_idx = sf->caller;
      int dest = sf->ret_dest;
      sp = caller_idx;
      if (dest >= 0) {
        call_stack[sp].locals[dest] = rv;
      }
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
    case OP_PUSH: {
      int src = instr.a;
      arg_stack[arg_p++] = call_stack[sp].locals[src];
    } break;
    case OP_CALL_EXTERN: {
      int dest = instr.a;
      int func_idx = instr.b;
      int nargs = instr.c;

      if (func_idx < 0 || (unsigned)func_idx >= CACHED_EXTERNS.length) {
        VM_ERRF("invalid extern index %d", func_idx);
      }
      Extern_Function callee = CACHED_EXTERNS.data[func_idx];

      int base = arg_p - nargs;
      if (base < 0) {
        VM_ERRF("not enough arguments on arg stack for extern call (need=%d "
                "have=%d)",
                nargs, arg_p);
      }

      Value value = libffi_dynamic_dispatch(callee, &arg_stack[base], nargs);
      /* pop the used args */
      arg_p = base;
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
      }

      int base = arg_p - nargs;
      if (base < 0) {
        VM_ERRF("not enough arguments on arg stack for call (need=%d have=%d)",
                nargs, arg_p);
      }

      int new_sp = sp + 1;
      call_stack[new_sp] = enter(callee, dest, sp);
      for (int i = 0; i < nargs; ++i) {
        call_stack[new_sp].locals[i] = arg_stack[i];
        call_stack[new_sp].locals[i] = arg_stack[base + i];
      }
      arg_p = 0;
      arg_p = base;
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
      if (caller_idx == ENTRY_POINT_CALLER) {
        break;
      }
      sp = caller_idx;
      if (ret_dest >= 0) {
        call_stack[sp].locals[ret_dest] = rv;
      }
    } break;
    default:
      VM_ERRF("unknown opcode %d", instr.op);
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

ffi_type type_to_ffi_type(Type *type) {
  switch (type->tag) {
  case TYPE_INT:
    return ffi_type_sint32;
  case TYPE_STRING:
    return ffi_type_pointer;
  case TYPE_VOID:
    return ffi_type_void;
  default:
    fprintf(stderr, "unable to use type: %s with libffi\n", type->name);
    exit(1);
  }
}

Extern_Function get_ffi_function_from_thir(Thir *thir) {
  LIST_FOREACH(CACHED_EXTERNS, cached) {
    if (cached.name == thir->extern_function.name) {
      return cached;
    }
  }

  const char *LIB_SEARCH_PATHS[] = {"libm.so.6",
                                    "libm.so",
                                    "libc.so.6",
                                    "libc.so",
                                    "/home/josh/source/c/bindings/libb.so",
                                    NULL};

  static struct {
    const char *name;
    void *handle;
  } open_libs[16] = {};
  static int open_libs_count = 0;

  void *handle = NULL;
  void *symbol = NULL;

  for (const char **p = LIB_SEARCH_PATHS; *p != NULL; ++p) {
    int found = 0;
    for (int i = 0; i < open_libs_count; ++i) {
      if (strcmp(open_libs[i].name, *p) == 0) {
        handle = open_libs[i].handle;
        found = 1;
        break;
      }
    }
    if (!found) {
      handle = dlopen(*p, RTLD_NOW);
      if (handle) {

        if (open_libs_count < (int)(sizeof(open_libs) / sizeof(open_libs[0]))) {
          open_libs[open_libs_count].name = *p;
          open_libs[open_libs_count].handle = handle;
          ++open_libs_count;
        } else {
          fprintf(stderr, "unable to open dynamic library for ffi: too many "
                          "libraries open\n");
          exit(1);
        }
      }
    }

    if (!handle) {
      continue;
    }

    dlerror();
    symbol = dlsym(handle, thir->extern_function.name);

    if (symbol) {
      break;
    }
  }

  if (!symbol) {
    fprintf(stderr, "unable to find symbol '%s' in system libraries\n",
            thir->extern_function.name);
    exit(1);
  }

  Function_Type *type = (Function_Type *)thir->type;

  Extern_Function extern_function = {.name = thir->extern_function.name,
                                     .parameters = {0},
                                     .return_type =
                                         type_to_ffi_type(type->returns),
                                     .index = CACHED_EXTERNS.length,
                                     .ptr = symbol,
                                     .original_return_type = type->returns};

  LIST_FOREACH(type->parameters, parameter) {
    LIST_PUSH(extern_function.parameters, type_to_ffi_type(parameter));
  }

  LIST_PUSH(CACHED_EXTERNS, extern_function);

  return extern_function;
}

Value libffi_dynamic_dispatch(Extern_Function function, Value *argv, int argc) {
  ffi_cif cif;

  if (!function.ptr) {
    fprintf(stderr, "[VM:FFI] error: function.ptr is (nil) for extern '%s'\n",
            function.name);
    exit(1);
  }

  size_t n_params = function.parameters.length;
  ffi_type *arg_types[n_params ? n_params : 1];
  void *arg_values[n_params ? n_params : 1];

  if ((size_t)argc < n_params) {
    fprintf(stderr,
            "[VM:FFI] attempted to call \"%s\" via 'extern', but too few "
            "arguments were provided. got=%d need=%zu\n",
            function.name, argc, n_params);
    exit(1);
  }

  for (size_t i = 0; i < n_params; ++i) {
    arg_types[i] = &function.parameters.data[i];
    Value *v = &argv[i];
    switch (v->type) {
    case VALUE_INTEGER:
      arg_values[i] = &v->integer;
      break;
    case VALUE_STRING:
      arg_values[i] = &v->string;
      break;
    default:
      fprintf(stderr, "[VM:FFI] unsupported argument type for extern call\n");
      exit(1);
    }
  }

  ffi_type *ffi_return_type = &function.return_type;

  int int_buf = 0;
  char *string_buf = NULL;
  void *return_buf = NULL;

  int return_type = function.original_return_type->tag;

  if (return_type == TYPE_INT) {
    return_buf = &int_buf;
  } else if (return_type == TYPE_STRING) {
    return_buf = &string_buf;
  } else if (return_type == TYPE_VOID) {
    return_buf = NULL;
  }

  int prep = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, (unsigned)n_params,
                          ffi_return_type, arg_types);
  if (prep != FFI_OK) {
    fprintf(stderr, "[VM:FFI] ffi_prep_cif failed: %d\n", prep);
    exit(1);
  }

  ffi_call(&cif, function.ptr, return_buf, arg_values);

  if (return_type == TYPE_INT) {
    return (Value){.type = VALUE_INTEGER, .integer = int_buf};
  } else if (return_type == TYPE_STRING) {
    return (Value){.type = VALUE_STRING, .string = string_buf};
  } else if (return_type == TYPE_VOID) {
    return (Value){.type = VALUE_VOID};
  }

  fprintf(stderr, "[VM:FFI] no return type was specified\n");
  exit(1);
}