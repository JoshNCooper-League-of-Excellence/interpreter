#include "tac.h"
#include "binding.h"
#include "lexer.h"
#include "list.h"
#include "thir.h"
#include "type.h"

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

// Collects member indices from a chained member access and returns the base
// expr. path is filled from leaf-to-root; function also builds root-to-leaf
// order.
static int collect_member_path(Thir *n, int leaf_to_root[], int max_depth,
                               Thir **out_base) {
  int depth = 0;
  Thir *cur = n;
  while (cur->tag == THIR_MEMBER_ACCESS) {
    assert(depth < max_depth && "member access chain too deep");
    leaf_to_root[depth++] = (int)cur->member_access.index;
    cur = cur->member_access.base;
  }
  *out_base = cur;
  return depth;
}

static int lower_member_rvalue(Thir *n, Function *fn, Module *m) {
  int leaf_to_root[32];
  Thir *base = NULL;
  int depth = collect_member_path(n, leaf_to_root, 32, &base);

  int obj;
  if (base->tag == THIR_VARIABLE) {
    obj = generate_temp(fn);
    EMIT_LOAD(&fn->code, obj, (int)base->binding->index);
  } else {
    // Non-variable base (e.g. call returning a struct)
    obj = lower_expression(base, fn, m);
  }

  // Traverse from root to leaf: reverse the leaf_to_root order.
  for (int i = depth - 1; i >= 0; --i) {
    int tmp = generate_temp(fn);
    EMIT_MEMBER_LOAD(&fn->code, tmp, obj, leaf_to_root[i]);
    obj = tmp;
  }
  return obj;
}

// Lower assignment into a (possibly chained) member LHS: a.b.c = rhs
static int lower_member_assignment(Thir *lhs, int rhs, Function *fn) {
  int leaf_to_root[32];
  Thir *base = NULL;
  int depth = collect_member_path(lhs, leaf_to_root, 32, &base);
  assert(depth > 0 && "expected member chain for member assignment");

  // Root-to-leaf order
  int path[32];
  for (int i = 0; i < depth; ++i)
    path[i] = leaf_to_root[depth - 1 - i];

  // Only variables are assignable as the root container for now.
  assert(base->tag == THIR_VARIABLE && "assignment to non-lvalue member base");

  int slot = (int)base->binding->index;

  // Load the top-level struct value
  int obj0 = generate_temp(fn);
  EMIT_LOAD(&fn->code, obj0, slot);

  // Build containers down to the parent of the leaf
  int containers[33];
  containers[0] = obj0;
  for (int i = 0; i < depth - 1; ++i) {
    int tmp = generate_temp(fn);
    EMIT_MEMBER_LOAD(&fn->code, tmp, containers[i], path[i]);
    containers[i + 1] = tmp;
  }

  // Write the rhs into the leaf field
  EMIT_MEMBER_STORE(&fn->code,
                    containers[depth - 1], // parent container
                    path[depth - 1],       // leaf index
                    rhs);

  // Write back up the chain
  for (int i = depth - 2; i >= 0; --i) {
    EMIT_MEMBER_STORE(&fn->code, containers[i], path[i], containers[i + 1]);
  }

  // Store updated top-level struct back to the variable slot
  EMIT_STORE(&fn->code, slot, containers[0]);

  return rhs; // assignment expression result
}

int lower_lvalue_slot(Thir *n) {
  switch (n->tag) {
  case THIR_VARIABLE:
    return (int)n->binding->index;
  default:
    char *msg;
    asprintf(&msg, "lower_lvalue: unsupported lvalue tag %d at %s", n->tag,
             lexer_span_to_string(n->span));
    fprintf(stderr, "%s\n", msg);
    free(msg);
    exit(EXIT_FAILURE);
  }
}

int lower_expression(Thir *n, Function *fn, Module *m) {
  assert(n && "null expression while lowering");
  switch (n->tag) {
  case THIR_MEMBER_ACCESS: {
    return lower_member_rvalue(n, fn, m);
  }
  case THIR_AGGREGATE_INITIALIZER: {
    int dest = generate_temp(fn);
    EMIT_ALLOCA(&fn->code, dest, n->type->index);
    LIST_FOREACH(n->aggregate_initializer.values, value) {
      int v = lower_expression(value, fn, m);
      EMIT_MEMBER_STORE(&fn->code, dest, __i, v);
    }
    return dest;
  }
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
    if (n->binary.op == TOKEN_ASSIGN) {
      int r = lower_expression(n->binary.right, fn, m);
      if (n->binary.left->tag == THIR_MEMBER_ACCESS) {
        return lower_member_assignment(n->binary.left, r, fn);
      }
      int l = lower_lvalue_slot(n->binary.left);
      EMIT_STORE(&fn->code, l, r);
      return l;
    }

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

    int nargs = n->call.arguments.length;

    LIST_FOREACH(n->call.arguments, argument) {
      int dest = lower_expression(argument, fn, m);
      EMIT_PUSH(&fn->code, dest);
    }

    int dest = generate_temp(fn);
    assert(n->call.callee && "null callee while lowering");

    if (n->call.callee->thir->tag == THIR_EXTERN) {
      EMIT_CALL_EXTERN(&fn->code, dest,
                       n->call.callee->thir->extern_function.index, nargs);
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
    exit(1);
  }
}

void lower_block(Thir *block, Function *fn, Module *m) {
  LIST_FOREACH(block->block, stmt) {
    switch (stmt->tag) {
    case THIR_BINARY: {
      // dot expressions assignment, ignored result.
      lower_expression(stmt, fn, m);
      break;
    }
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
      int slot = (int)fn->n_locals++;
      stmt->binding->index = (size_t)slot;
      int src = lower_expression(stmt->variable_initializer, fn, m);
      EMIT_STORE(&fn->code, slot, src);
      break;
    }
    case THIR_CALL:
    case THIR_LITERAL:
      lower_expression(stmt, fn, m);
      break;
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
  case OP_ALLOCA:
    sb_appendf(sb, "ALLOCA t%d, type=%d", i->a, i->b);
    break;
  case OP_CALL_EXTERN:
    sb_appendf(sb, "CALL_EXTERN t%d, ext%d, nargs=%d", i->a, i->b, i->c);
    break;
  case OP_MEMBER_LOAD:
    sb_appendf(sb, "MEMBER_LOAD t%d, obj=t%d, idx=%d", i->a, i->b, i->c);
    break;
  case OP_MEMBER_STORE:
    sb_appendf(sb, "MEMBER_STORE obj=t%d, idx=%d, src=t%d", i->a, i->b, i->c);
    break;
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
  case OP_PUSH:
    sb_appendf(sb, "ARG i=%d, src=t%d", i->a, i->b);
    break;
  default:
    sb_appendf(sb, "UNKNOWN_OP %d", i->op);
    break;
  }
}

void print_module(Module *m, String_Builder *sb) {

  sb_append(sb, "TYPES:\n");
  for (size_t i = 0; i < m->types.length; ++i) {
    Type *type = m->types.data[i];
    if (type->tag == TYPE_STRUCT) {
      Struct_Type *struct_type = (Struct_Type *)type;
      sb_appendf(sb, "[%zu]: struct %s {\n", i, type->name);
      LIST_FOREACH(struct_type->members, member) {
        sb_appendf(sb, "  [%zu]: %s\n", __i, member.type->name);
      }
      sb_append(sb, "}\n");
    }
  }

  sb_append(sb, "CONSTANTS:\n");
  LIST_FOREACH(m->constants, constant) {
    sb_appendf(sb, "\t[%d]: { type: %s, value: \"%s\" }\n", __i,
               constant.type == CONST_INTEGER ? "int" : "string",
               constant.value);
  }

  sb_append(sb, "FUNCTIONS:\n");
  LIST_FOREACH(m->functions, function) {
    sb_appendf(sb, "  %s:\n", function->name);
    for (unsigned i = 0; i < function->code.length; ++i) {
      sb_append(sb, "\t");
      print_instr(&function->code.data[i], sb);
      sb_append(sb, "\n");
    }
  }
}

void module_init(Module *m, Context *context) {
  m->types = context->type_table;
}