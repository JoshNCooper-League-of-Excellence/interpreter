#include "vm.h"
#include "list.h"
#include "string_builder.h"
#include "tac.h"
#include "type.h"
#include <dlfcn.h>
#include <ffi.h>

// this is the integer type the entire VM runs on (data-wise)
typedef signed long long integer;

#define VM_ERRF(msg, ...)                                                                                                   \
  fprintf(stderr, "[VM]: " msg "\n" __VA_OPT__(, ) __VA_ARGS__);                                                            \
  exit(1);

static inline Stack_Frame enter(Function *fn, integer ret_dest, integer caller) {
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

static inline char *fix_escape_characters(const char *s) {
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

  // collect constants into 'Value's
  LIST_FOREACH(m->constants, constant) {
    if (constant.type == CONST_TYPE_INT) {
      constants[__i] = (Value){.integer = atoi(constant.value), .tag = VALUE_INTEGER};
    } else if (constant.type == CONST_TYPE_STRING) {
      char *fixed = fix_escape_characters(constant.value);
      unsigned length = strlen(fixed);
      Value value = {.tag = VALUE_POINTER, .pointer = {.elements = fixed, .length = length}};
      constants[__i] = value;
    }
  }

  Stack_Frame call_stack[256];
  for (integer i = 0; i < 256; ++i) {
    Stack_Frame *frame = &call_stack[i];
    frame->uid = i;
  }

  integer sp = 0; // stack pointer (into call_stack)

  Value arg_stack[64];
  unsigned arg_p = 0;

#define ENTRY_POINT_CALLER -1

  Function *entry = m->entry_point;
  call_stack[0] = enter(entry, ENTRY_POINT_CALLER, ENTRY_POINT_CALLER);

  Stack_Frame *sf = &call_stack[sp];
  Instr instr;

  for (;;) {
    if (sf->ip >= sf->fn->code.length) {
      goto L_RETURN_TO_CALLER;
    }
    instr = sf->fn->code.data[sf->ip++];

    switch (instr.op) {
    case OP_ALLOCA: {
      signed dest = instr.a;
      signed ty_idx = instr.b;
      signed length = instr.c;
      Type *type = m->types.data[ty_idx];
      if (length == 0) {
        sf->locals[dest] = default_value_of_type(type, sf->uid);
      } else {
        sf->locals[dest] = default_array_of_type(type, sf->uid, length);
      }
      continue;
    }

    case OP_MEMBER_LOAD_INDIRECT: {
      signed dest = instr.a;
      signed src = instr.b;
      signed ptr = instr.c;
      integer idx = sf->locals[ptr].integer;

      Value *val = &sf->locals[src];
      if (val->tag == VALUE_STRUCT) {
        sf->locals[dest] = val->$struct.members[idx];
      } else if (val->tag == VALUE_POINTER) {
        if (val->pointer.pointee == POINTEE_VALUE) {
          Value *elements = val->pointer.elements;
          sf->locals[dest] = elements[idx];
        } else {
          VM_ERRF("OP_MEMBER_LOAD not supported for unmanged pointers or arrays yet");
        }
      }

      continue;
    }

    case OP_MEMBER_LOAD: {
      signed dest = instr.a;
      signed src = instr.b;
      signed idx = instr.c;
      Value *val = &sf->locals[src];
      if (val->tag == VALUE_STRUCT) {
        sf->locals[dest] = val->$struct.members[idx];
      } else if (val->tag == VALUE_POINTER) {
        if (val->pointer.pointee == POINTEE_VALUE) {
          Value *elements = val->pointer.elements;
          sf->locals[dest] = elements[idx];
        } else {
          VM_ERRF("OP_MEMBER_LOAD not supported for unmanged pointers or arrays yet");
        }
      }

      continue;
    }

    case OP_MEMBER_STORE: {
      signed struct_slot = instr.a;
      signed member_idx = instr.b;
      signed src = instr.c;
      Value *val = &sf->locals[struct_slot];
      if (val->tag == VALUE_STRUCT) {
        val->$struct.members[member_idx] = sf->locals[src];
      } else if (val->tag == VALUE_POINTER) {
        if (val->pointer.pointee == POINTEE_VALUE) {
          ((Value *)val->pointer.elements)[member_idx] = sf->locals[src];
        } else {
          VM_ERRF("OP_MEMBER_STORE not supported for unmanged pointers or arrays yet");
        }
      }
      continue;
    }

    case OP_MEMBER_STORE_INDIRECT: {
      signed temp = instr.a;
      signed ptr = instr.b;
      signed src = instr.c;
      Value *val = &sf->locals[temp];
      integer index = sf->locals[ptr].integer;

      if (val->tag == VALUE_STRUCT) {
        val->$struct.members[index] = sf->locals[src];
      } else if (val->tag == VALUE_POINTER) {
        if (val->pointer.pointee == POINTEE_VALUE) {
          ((Value *)val->pointer.elements)[index] = sf->locals[src];
        } else {
          VM_ERRF("OP_MEMBER_STORE not supported for unmanged pointers or arrays yet");
        }
      }
      continue;
    }

    case OP_CONST: {
      signed dest = instr.a;
      signed cidx = instr.b;
      sf->locals[dest] = constants[cidx];
      continue;
    }

    case OP_LOAD: {
      signed dest = instr.a;
      signed slot = instr.b;
      sf->locals[dest] = sf->locals[slot];
      continue;
    }

    case OP_STORE: {
      signed slot = instr.a;
      signed src = instr.b;
      sf->locals[slot] = sf->locals[src];
      continue;
    }

    case OP_ADD: {
      signed d = instr.a, l = instr.b, r = instr.c;
      sf->locals[d].integer = sf->locals[l].integer + sf->locals[r].integer;
      sf->locals[d].tag = VALUE_INTEGER;
      continue;
    }

    case OP_SUB: {
      signed d = instr.a, l = instr.b, r = instr.c;
      sf->locals[d].integer = sf->locals[l].integer - sf->locals[r].integer;
      sf->locals[d].tag = VALUE_INTEGER;
      continue;
    }

    case OP_MUL: {
      signed d = instr.a, l = instr.b, r = instr.c;
      sf->locals[d].integer = sf->locals[l].integer * sf->locals[r].integer;
      sf->locals[d].tag = VALUE_INTEGER;
      continue;
    }

    case OP_DIV: {
      signed d = instr.a, l = instr.b, r = instr.c;
      integer rvalue = sf->locals[r].integer;
      if (rvalue == 0) {
        VM_ERRF("attempted to divide by zero. (div) ip=%d, fn=%s", sf->ip, sf->fn->name);
      }
      sf->locals[d].integer = sf->locals[l].integer / rvalue;
      sf->locals[d].tag = VALUE_INTEGER;
      continue;
    }

    case OP_PUSH: {
      signed src = instr.a;
      arg_stack[arg_p++] = sf->locals[src];
      continue;
    }

    case OP_CALL_EXTERN: {
      signed dest = instr.a;
      signed func_idx = instr.b;
      signed nargs = instr.c;

      if (func_idx < 0 || (unsigned)func_idx >= CACHED_EXTERNS.length) {
        VM_ERRF("invalid extern index %d", func_idx);
      }
      Extern_Function callee = CACHED_EXTERNS.data[func_idx];

      integer base = arg_p - nargs;
      if (base < 0) {
        VM_ERRF("not enough arguments on arg stack for extern call (need=%d "
                "have=%d)",
                nargs, arg_p);
      }

      Value value = libffi_dynamic_dispatch(callee, &arg_stack[base], nargs);
      arg_p = base; // pop
      sf->locals[dest] = value;
      continue;
    }

    case OP_CALL: {
      signed dest = instr.a;
      signed func_idx = instr.b;
      signed nargs = instr.c;

      if (func_idx < 0 || (unsigned)func_idx >= m->functions.length) {
        VM_ERRF("invalid function index %d", func_idx);
      }
      Function *callee = m->functions.data[func_idx];

      if (sp + 1 >= (int)(sizeof(call_stack) / sizeof(call_stack[0]))) {
        VM_ERRF("call stack overflow");
      }

      integer base = arg_p - nargs;
      if (base < 0) {
        VM_ERRF("not enough arguments on arg stack for call (need=%d have=%d)", nargs, arg_p);
      }

      integer new_sp = sp + 1;
      call_stack[new_sp] = enter(callee, dest, sp);
      for (integer i = 0; i < nargs; ++i) {
        call_stack[new_sp].locals[i] = arg_stack[base + i];
      }
      arg_p = base; // pop args
      sp = new_sp;
      sf = &call_stack[sp];

      continue;
    }

    case OP_RET: {
      signed src = instr.a;
      Value rv = {.tag = VALUE_VOID};
      if (src >= 0) {
        rv = sf->locals[src];
      }
      integer caller_idx = sf->caller;
      integer ret_dest = sf->ret_dest;
      leave(sf);
      if (caller_idx == ENTRY_POINT_CALLER) {
        goto L_EXIT;
      }
      sp = caller_idx;
      sf = &call_stack[sp];
      if (ret_dest >= 0) {
        sf->locals[ret_dest] = rv;
      }
      continue;
    }

    case OP_NEGATE: {
      signed dest = instr.a, src = instr.b;
      sf->locals[dest].integer = -sf->locals[src].integer;
      sf->locals[dest].tag = VALUE_INTEGER;
      continue;
    }

    case OP_LOGICAL_OR: {
      signed d = instr.a, l = instr.b, r = instr.c;
      sf->locals[d].integer = (sf->locals[l].integer || sf->locals[r].integer);
      sf->locals[d].tag = VALUE_INTEGER;
      continue;
    }

    case OP_LOGICAL_AND: {
      signed d = instr.a, l = instr.b, r = instr.c;
      sf->locals[d].integer = (sf->locals[l].integer && sf->locals[r].integer);
      sf->locals[d].tag = VALUE_INTEGER;
      continue;
    }

    case OP_SHIFT_LEFT: {
      signed d = instr.a, l = instr.b, r = instr.c;
      sf->locals[d].integer = (sf->locals[l].integer << sf->locals[r].integer);
      sf->locals[d].tag = VALUE_INTEGER;
      continue;
    }

    case OP_SHIFT_RIGHT: {
      signed d = instr.a, l = instr.b, r = instr.c;
      sf->locals[d].integer = (sf->locals[l].integer >> sf->locals[r].integer);
      sf->locals[d].tag = VALUE_INTEGER;
      continue;
    }

    case OP_XOR: {
      signed d = instr.a, l = instr.b, r = instr.c;
      sf->locals[d].integer = (sf->locals[l].integer ^ sf->locals[r].integer);
      sf->locals[d].tag = VALUE_INTEGER;
      continue;
    }

    case OP_BIT_OR: {
      signed d = instr.a, l = instr.b, r = instr.c;
      sf->locals[d].integer = (sf->locals[l].integer | sf->locals[r].integer);
      sf->locals[d].tag = VALUE_INTEGER;
      continue;
    }

    case OP_BIT_AND: {
      signed d = instr.a, l = instr.b, r = instr.c;
      sf->locals[d].integer = (sf->locals[l].integer & sf->locals[r].integer);
      sf->locals[d].tag = VALUE_INTEGER;
      continue;
    }

    case OP_MODULO: {
      signed d = instr.a, l = instr.b, r = instr.c;
      integer rvalue = sf->locals[r].integer;
      if (rvalue == 0) {
        VM_ERRF("attempted to divide by zero. (modulo) ip=%d, fn=%s", sf->ip, sf->fn->name);
      }
      sf->locals[d].integer = (sf->locals[l].integer % rvalue);
      sf->locals[d].tag = VALUE_INTEGER;
      continue;
    }

    case OP_EQUALS: {
      signed d = instr.a, l = instr.b, r = instr.c;
      sf->locals[d].integer = (sf->locals[l].integer == sf->locals[r].integer);
      sf->locals[d].tag = VALUE_INTEGER;
      continue;
    }

    case OP_NOT_EQUALS: {
      signed d = instr.a, l = instr.b, r = instr.c;
      sf->locals[d].integer = (sf->locals[l].integer != sf->locals[r].integer);
      sf->locals[d].tag = VALUE_INTEGER;
      continue;
    }

    case OP_LESS: {
      signed d = instr.a, l = instr.b, r = instr.c;
      sf->locals[d].integer = (sf->locals[l].integer < sf->locals[r].integer);
      sf->locals[d].tag = VALUE_INTEGER;
      continue;
    }

    case OP_GREATER: {
      signed d = instr.a, l = instr.b, r = instr.c;
      sf->locals[d].integer = (sf->locals[l].integer > sf->locals[r].integer);
      sf->locals[d].tag = VALUE_INTEGER;
      continue;
    }

    case OP_LOGICAL_NOT: {
      signed d = instr.a, s = instr.b;
      sf->locals[d].integer = !sf->locals[s].integer;
      sf->locals[d].tag = VALUE_INTEGER;
      continue;
    }

    case OP_BIT_NOT: {
      signed d = instr.a, s = instr.b;
      sf->locals[d].integer = ~sf->locals[s].integer;
      sf->locals[d].tag = VALUE_INTEGER;
      continue;
    }

    case OP_JUMP_IF: {
      signed cond = instr.a;
      signed target = instr.b;
      if (sf->locals[cond].integer) {
        sf->ip += target;
      }
      continue;
    }

    case OP_JUMP: {
      signed target = instr.a;
      sf->ip += target;
      continue;
    }
    }

  L_RETURN_TO_CALLER:
    // Ran off the end without an explicit return
    if (sf->caller == ENTRY_POINT_CALLER) {
      goto L_EXIT;
    }
    {
      Value rv = sf->locals[0];
      integer caller_idx = sf->caller;
      integer dest = sf->ret_dest;
      leave(sf);
      sp = caller_idx;
      sf = &call_stack[sp];
      if (dest >= 0) {
        sf->locals[dest] = rv;
      }
    }
    continue;
  }

L_EXIT:
  leave(&call_stack[0]);
  return;
}

void print_value(Value *value, String_Builder *sb) {
  switch (value->tag) {
  case VALUE_INTEGER:
    sb_appendf(sb, "%d", value->integer);
    break;
  case VALUE_POINTER:
    sb_appendf(sb, "{ ptr = %p, length = %d, managed = %d }", value->pointer.elements, value->pointer.length,
               value->pointer.pointee == POINTEE_VALUE);
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

Value default_array_of_type(Type *type, unsigned owner_uid, unsigned long long length) {
  Value *data;
  Value array = {.tag = VALUE_POINTER,
                 .pointer = {
                     .length = length,
                     .elements = data = malloc(sizeof(Value) * length),
                     .pointee = POINTEE_VALUE,
                 }};
  for (unsigned long long i = 0; i < length; ++i) {
    data[i] = default_value_of_type(type, owner_uid);
  }
  return array;
}

Value default_value_of_type(Type *type, unsigned owner_uid) {
  switch (type->tag) {
  case TYPE_BYTE:
    if (type_is_pointer_of_depth(type, 1)) {
      return (Value){.tag = VALUE_POINTER, .pointer = {nullptr, 0, POINTEE_RAW}, .type = type};
    }
  // fall through intentional
  case TYPE_BOOL:
  case TYPE_INT: {
    return (Value){
        .tag = VALUE_INTEGER,
        .integer = 0,
        .type = type,
    };
  }
  case TYPE_STRUCT: {
    Struct_Type *struct_type = (Struct_Type *)type;
    Value v = {
        .owner_uid = owner_uid,
        .tag = VALUE_STRUCT,
        .$struct.length = struct_type->members.length,
        .$struct.members = calloc(struct_type->members.length, sizeof(Value)),
        .type = type,
    };
    for (size_t i = 0; i < struct_type->members.length; ++i) {
      Type *type = struct_type->members.data[i].type;
      v.$struct.members[i] = default_value_of_type(type, owner_uid);
    }
    return v;
  }
  case TYPE_FUNCTION:
  case TYPE_VOID:
    return (Value){.tag = VALUE_VOID, .type = type};
    break;

    break;
  }
}

void value_free(Value *value, unsigned owner_uid) {
  if (value->tag == VALUE_STRUCT && value->owner_uid == owner_uid) {
    for (unsigned i = 0; i < value->$struct.length; ++i) {
      value_free(&value->$struct.members[i], owner_uid);
    }
    free(value->$struct.members);
  }
  *value = (Value){0};
}

void leave(Stack_Frame *frame) {
  if (!frame->locals) {
    return;
  }

  for (integer i = 0; i < frame->n_locals; ++i) {
    Value *value = &frame->locals[i];
    value_free(value, frame->uid);
  }
}

ffi_type type_to_ffi_type(Type *type) {
  // both arrays and pointers can be treated as pointers
  if (type_has_extensions(type)) {
    return ffi_type_pointer;
  }

  switch (type->tag) {
  case TYPE_BYTE:
  case TYPE_BOOL:
  case TYPE_INT:
    return ffi_type_sint32;
  case TYPE_VOID:
    return ffi_type_void;
  case TYPE_FUNCTION:
  case TYPE_STRUCT:
    fprintf(stderr, "unable to use type: %s with libffi\n", type->name);
    exit(1);
    break;
  }
}

Extern_Function get_ffi_function_from_thir(Thir *thir) {
  LIST_FOREACH(CACHED_EXTERNS, cached) {
    if (cached.name == thir->extern_function.name) {
      return cached;
    }
  }

  const char *LIB_SEARCH_PATHS[] = {"libm.so.6", "libm.so", "libc.so.6", "libc.so", "/home/josh/source/c/bindings/libb.so",
                                    NULL};

  static struct {
    const char *name;
    void *handle;
  } open_libs[16] = {};
  static integer open_libs_count = 0;

  void *handle = NULL;
  void *symbol = NULL;

  for (const char **p = LIB_SEARCH_PATHS; *p != NULL; ++p) {
    integer found = 0;
    for (integer i = 0; i < open_libs_count; ++i) {
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
    fprintf(stderr, "unable to find symbol '%s' in system libraries\n", thir->extern_function.name);
    exit(1);
  }

  Function_Type *type = (Function_Type *)thir->type;

  Extern_Function extern_function = {.name = thir->extern_function.name,
                                     .parameters = {0},
                                     .return_type = type_to_ffi_type(type->returns),
                                     .index = CACHED_EXTERNS.length,
                                     .ptr = symbol,
                                     .original_return_type = type->returns,
                                     .function_type = type};

  LIST_FOREACH(type->parameters, parameter) { LIST_PUSH(extern_function.parameters, type_to_ffi_type(parameter)); }

  LIST_PUSH(CACHED_EXTERNS, extern_function);

  return extern_function;
}

Value libffi_dynamic_dispatch(Extern_Function function, Value *argv, int argc) {
  ffi_cif cif;

  if (!function.ptr) {
    fprintf(stderr, "[VM:FFI] error: function.ptr is (nil) for extern '%s'\n", function.name);
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
    switch (v->tag) {
    case VALUE_INTEGER:
      arg_values[i] = &v->integer;
      break;
    case VALUE_POINTER:
      arg_values[i] = &v->pointer.elements;
      break;
    default:
      fprintf(stderr,
              "[VM:FFI] unsupported argument type 'Value_Type(%d)' for extern "
              "call (%s)\n",
              v->tag, function.name);
      exit(1);
    }
  }

  ffi_type *ffi_return_type = &function.return_type;

  integer int_buf = 0;
  char *ptr_buf = NULL;
  void *return_buf = NULL;

  Type *return_type = function.original_return_type;
  bool returns_pointer = type_is_pointer(function.original_return_type);

  if (return_type->tag == TYPE_INT) {
    return_buf = &int_buf;
  } else if (returns_pointer) {
    return_buf = &ptr_buf;
  } else if (return_type->tag == TYPE_VOID) {
    return_buf = NULL;
  }

  ffi_status prep = ffi_prep_cif(&cif, FFI_DEFAULT_ABI, (unsigned)n_params, ffi_return_type, arg_types);
  if (prep != FFI_OK) {
    fprintf(stderr, "[VM:FFI] ffi_prep_cif failed: %d\n", prep);
    exit(1);
  }

  ffi_call(&cif, function.ptr, return_buf, arg_values);

  if (return_type->tag == TYPE_INT) {
    return (Value){.tag = VALUE_INTEGER, .integer = int_buf, .type = return_type};
  } else if (returns_pointer) {
    return (Value){.tag = VALUE_POINTER,
                   .type = return_type,
                   .pointer = {
                       .elements = ptr_buf,
                       .length = 1,
                       .pointee = POINTEE_RAW,
                   }};
  } else if (return_type->tag == TYPE_VOID) {
    return (Value){.tag = VALUE_VOID, .type = return_type};
  }

  fprintf(stderr, "[VM:FFI] no return type was specified\n");
  exit(1);
}
