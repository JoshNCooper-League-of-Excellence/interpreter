#include "tac.h"
#include "binding.h"
#include "lexer.h"
#include "list.h"
#include "thir.h"

/* Notes:
   - This lowering uses binding->index for slots and fn->n_locals to allocate
   temps.

   - Keep bindings' index assignment consistent between typing and lowering or
     assign all per-function binding indices in lower_function.
*/

unsigned add_constant(Module *m, Thir *thir) {
  Constant_Type type = CONST_INTEGER;

  if (thir->type->tag == TYPE_STRING) {
    type = CONST_STRING;
  } else if (thir->type->tag == TYPE_INT) {
    type = CONST_INTEGER;
  } else {
    assert(false && "[TAC]: invalid constant type, expected 'int' or 'string'");
  }

  const char *value = thir->literal.value;

  // We already have a constant that matches this one, re-use it.
  LIST_FOREACH(m->constants, constant) {
    if (strcmp(constant.value, value) == 0 && constant.type == type) {
      return __i;
    }
  }

  Constant constant = {
      .type = type,
      .value = value,
  };

  unsigned index = m->constants.length;
  LIST_PUSH(m->constants, constant);

  return index;
}

unsigned push_function(Module *m, Function *f) {
  unsigned index = m->functions.length;
  LIST_PUSH(m->functions, f);
  return index;
}

int generate_temp(Function *fn) { return (int)(fn->n_locals++); }

DEFINE_LIST(int);

int lower_expression(Thir *n, Function *fn, Module *m) {
  assert(n && "null expression while lowering");
  switch (n->tag) {
  case THIR_LITERAL: {
    int dest = generate_temp(fn);
    unsigned cidx = add_constant(m, n);
    EMIT_CONST(&fn->code, dest, (int)cidx);
    return dest;
  }
  case THIR_VARIABLE: {
    int dest = generate_temp(fn);
    size_t slot = n->binding->index;
    EMIT_LOAD(&fn->code, dest, (int)slot);
    return dest;
  }
  case THIR_BINARY: {
    int l = lower_expression(n->binary.left, fn, m);
    int r = lower_expression(n->binary.right, fn, m);
    int dest = generate_temp(fn);
    switch (n->binary.op) {
    case TOKEN_PLUS:
      EMIT_ADD(&fn->code, dest, l, r);
      break;
    case TOKEN_MINUS:
      EMIT_SUB(&fn->code, dest, l, r);
      break;
    case TOKEN_STAR:
      EMIT_MUL(&fn->code, dest, l, r);
      break;
    case TOKEN_SLASH:
      EMIT_DIV(&fn->code, dest, l, r);
      break;
    default:
      fprintf(stderr, "lower_expression: unhandled binary op %d\n",
              n->binary.op);
      break;
    }
    return dest;
  }
  case THIR_CALL: {
    int nargs = (int)n->call.arguments.length;

    for (unsigned i = 0; i < n->call.arguments.length; ++i) {
      int dest = lower_expression(n->call.arguments.data[i], fn, m);
      EMIT_ARG(&fn->code, i, dest);
    }

    int dest = generate_temp(fn);

    assert(n->call.callee && "null callee while lowering");

    if (n->call.callee->thir->tag == THIR_EXTERN) {
      Instr instr = {.op = OP_CALL_EXTERN, .a = dest, .b = n->call.callee->thir->extern_function.index, .c = nargs};
      EMIT(&fn->code, instr);
    } else {
      int func_idx = (int)n->call.callee->index;
      EMIT_CALL(&fn->code, dest, func_idx, nargs);
    }

    return dest;
  }
  case THIR_UNARY: {
    int v = lower_expression(n->unary.operand, fn, m);
    int dest = generate_temp(fn);
    // We don't even have unary operators defined so nothing to do here
    EMIT_ADD(&fn->code, dest, v, 0);
    return dest;
  }
  default:
    fprintf(stderr, "lower_expr: unhandled THIR tag %d\n", n->tag);
    return 0;
  }
}

void lower_block(Thir *block, Function *fn, Module *m) {
  LIST_FOREACH(block->block, stmt) {
    switch (stmt->tag) {
    case THIR_RETURN: {
      if (stmt->return_value) {
        int tmp = lower_expression(stmt->return_value, fn, m);
        EMIT_RET(&fn->code, tmp);
      } else {
        EMIT_RET(&fn->code, -1);
      }
      break;
    }
    case THIR_VARIABLE: {
      int src = lower_expression(stmt->variable_initializer, fn, m);
      EMIT_STORE(&fn->code, (int)stmt->binding->index, src);
      break;
    }
    case THIR_CALL:
    case THIR_LITERAL:
      lower_expression(stmt, fn, m);
      return;
    default: {
      assert(false && "unexpected THIR node in block while lowering");
      break;
    }
    }
  }
}

void lower_function(Thir *fnode, Module *m) {
  assert(fnode && fnode->tag == THIR_FUNCTION &&
         "lower_function: got non function node");
  Function *fn = calloc(1, sizeof(Function));
  fn->name = fnode->function.name;
  fn->n_locals = 0;
  fn->param_count = fnode->function.parameters.length;
  fn->const_start = m->constants.length;

  if (strcmp(fn->name, "main") == 0) {
    m->entry_point = fn;
  }

  // TODO: do we want to do this? we could easily get overlapping indices,
  // but I think everything's been resolved already?
  for (unsigned i = 0; i < fnode->function.parameters.length; ++i) {
    Binding *b = fnode->function.parameters.data[i];
    assert(b && "got invalid binding in parameter list while lowering");
    b->index = i;
  }

  fn->n_locals = fn->param_count;

  lower_block(fnode->function.block, fn, m);

  // ensure we always return
  if (fn->code.length == 0 || fn->code.data[fn->code.length - 1].op != OP_RET) {
    EMIT_RET(&fn->code, 0);
  }

  push_function(m, fn);
}

void lower_program(Thir *program, Module *m) {
  assert(program && program->tag == THIR_PROGRAM &&
         "unexpected node while lowering, expected THIR_PROGRAM");

  LIST_INIT(m->functions);
  LIST_INIT(m->constants);

  LIST_FOREACH(program->program, f) {
    if (f->tag == THIR_EXTERN) {
      continue; // We just ignore it.
    } else {
      assert(f->tag == THIR_FUNCTION &&
             "unexpected node type while lowering. expected THIR_FUNCTION");
      lower_function(f, m);
    }
  }
}

void print_instr(Instr *i, String_Builder *sb) {
  switch (i->op) {
  case OP_CONST:
    sb_appendf(sb, "CONST t%d, c%d", i->a, i->b);
    break;
  case OP_LOAD:
    sb_appendf(sb, "LOAD t%d, slot%d", i->a, i->b);
    break;
  case OP_STORE:
    sb_appendf(sb, "STORE slot%d, t%d", i->a, i->b);
    break;
  case OP_ADD:
    sb_appendf(sb, "ADD t%d, t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_SUB:
    sb_appendf(sb, "SUB t%d, t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_MUL:
    sb_appendf(sb, "MUL t%d, t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_DIV:
    sb_appendf(sb, "DIV t%d, t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_CALL:
    sb_appendf(sb, "CALL t%d, fn%d, nargs=%d", i->a, i->b, i->c);
    break;
  case OP_RET:
    sb_appendf(sb, "RET t%d", i->a);
    break;
  case OP_ARG:
    sb_appendf(sb, "ARG i=%d, src=t%d", i->a, i->b);
    break;
  default:
    sb_appendf(sb, "UNKNOWN_OP %d", i->op);
    break;
  }
}

void print_module(Module *m, String_Builder *sb) {

  sb_append(sb, "CONSTANTS:\n");
  LIST_FOREACH(m->constants, constant) {
    sb_appendf(sb, "\t[%d]: { type: %s, value: \"%s\" }\n", __i,
               constant.type == CONST_INTEGER ? "int" : "string",
               constant.value);
  }

  sb_append(sb, "FUNCTIONS:\n");
  LIST_FOREACH(m->functions, function) {
    sb_appendf(sb, "  %s:\n", function->name);
    for (int i = 0; i < function->code.length; ++i) {
      sb_append(sb, "\t");
      print_instr(&function->code.data[i], sb);
      sb_append(sb, "\n");
    }
  }
}