#include "tac.h"
#include "ast.h"
#include "binding.h"
#include "lexer.h"
#include "list.h"
#include "thir.h"
#include "type.h"

/**
 * TAC quick reference (for VM/compiler implementers)
 *
 * Value model
 * - Integers: signed 64-bit ("integer"). Booleans are integers 0/1.
 * - Strings: C strings (char*). String constants are unescaped at load time.
 * - Structs: fixed-length aggregates; member indices are 0-based.
 *
 * Execution model
 * - Each function executes with a locals array: slots [0..param_count-1] are
 *   parameters, additional slots are temporaries/locals allocated sequentially.
 * - Instructions are three-address with operands a, b, c. Unless stated,
 *   binary ops follow: dest=a, left=b, right=c.
 * - Control flow uses relative jumps measured from the NEXT instruction
 *   (i.e., target_ip = ip + offset).
 *
 * Constants
 * - Module-wide pool. CONST: a=dest, b=constIndex.
 *
 * Memory/locals
 * - ALLOCA: a=dest, b=typeIndex → dest receives default value of type.
 * - LOAD:   a=dest, b=slot.
 * - STORE:  a=slot, b=src.
 * - MEMBER_LOAD:  a=dest, b=objSlot, c=memberIndex.
 * - MEMBER_STORE: a=objSlot, b=memberIndex, c=src.
 *
 * Calls
 * - PUSH: a=src pushes an argument; arguments are pushed left-to-right.
 * - CALL: a=dest, b=functionIndex, c=nargs.
 *         Callee receives args in locals[0..nargs-1]; return goes to dest.
 * - CALL_EXTERN: a=dest, b=externIndex, c=nargs; same calling convention.
 * - RET: a=srcTemp or -1 for void.
 *
 * Arithmetic and logic (all integer semantics)
 * - ADD/SUB/MUL/DIV/MODULO/SHIFT_LEFT/SHIFT_RIGHT/BIT_AND/BIT_OR/XOR.
 * - EQUALS/NOT_EQUALS/LESS/GREATER produce 0/1.
 * - LOGICAL_OR/LOGICAL_AND: truthy if nonzero. LOGICAL_NOT: 1 if zero else 0.
 * - NEGATE: arithmetic negate. BIT_NOT: bitwise not.
 *
 * Control flow
 * - JUMP: a=relativeOffset.
 * - JUMP_IF: a=cond, b=relativeOffset (taken if cond != 0).
 *
 * Errors (VM-defined)
 * - Division or modulo by zero is a runtime error.
 * - Out-of-bounds member access and invalid indices are runtime errors.
 */

unsigned add_constant(Module *m, Thir *thir) {
  Constant_Type type = CONST_TYPE_INT;

  // convert "true" and "false" to 1 and 0
  bool is_bool = thir->type->tag == TYPE_BOOL;

  switch (thir->type->tag) {
  case TYPE_INT:
    type = CONST_TYPE_INT;
    break;
  case TYPE_BYTE:
    if (type_is_pointer_of_depth(thir->type, 1)) {
      type = CONST_TYPE_STRING;
    } else {
      goto L_FAILURE;
    }
    break;
  case TYPE_BOOL:
    type = CONST_TYPE_INT;
    break;
  L_FAILURE:
  case TYPE_STRUCT:
  case TYPE_VOID:
  case TYPE_FUNCTION:
    assert(false && "[TAC]: invalid constant type, expected 'int' or 'string'");
    break;
  }

  const char *value = thir->literal.value;

  if (is_bool) {
    if (strncmp("true", value, 4) == 0) {
      value = "1";
    } else if (strncmp("false", value, 5) == 0) {
      value = "0";
    } else {
      assert(false && "[TAC]: invalid constant bool, expected 'true' or 'false'");
    }
  }

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

int collect_member_path(Thir *n, int leaf_to_root[], int max_depth, Thir **out_base) {
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

int lower_member_rvalue(Thir *n, Function *fn, Module *m) {
  int leaf_to_root[32];
  Thir *base = NULL;
  int depth = collect_member_path(n, leaf_to_root, 32, &base);

  int obj;
  if (base->tag == THIR_VARIABLE) {
    obj = generate_temp(fn);
    EMIT_LOAD(fn->code, obj, (int)base->binding->index);
  } else {
    // Non-variable base (e.g. call returning a struct)
    obj = lower_expression(base, fn, m);
  }

  // Traverse from root to leaf: reverse the leaf_to_root order.
  for (int i = depth - 1; i >= 0; --i) {
    int tmp = generate_temp(fn);
    EMIT_MEMBER_LOAD(fn->code, tmp, obj, leaf_to_root[i]);
    obj = tmp;
  }
  return obj;
}

int lower_member_assignment(Thir *lhs, int rhs, Function *fn) {
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
  EMIT_LOAD(fn->code, obj0, slot);

  // Build containers down to the parent of the leaf
  int containers[33];
  containers[0] = obj0;
  for (int i = 0; i < depth - 1; ++i) {
    int tmp = generate_temp(fn);
    EMIT_MEMBER_LOAD(fn->code, tmp, containers[i], path[i]);
    containers[i + 1] = tmp;
  }

  // Write the rhs into the leaf field
  EMIT_MEMBER_STORE(fn->code,
                    containers[depth - 1], // parent container
                    path[depth - 1],       // leaf index
                    rhs);

  // Write back up the chain
  for (int i = depth - 2; i >= 0; --i) {
    EMIT_MEMBER_STORE(fn->code, containers[i], path[i], containers[i + 1]);
  }

  // Store updated top-level struct back to the variable slot
  EMIT_STORE(fn->code, slot, containers[0]);

  return rhs; // assignment expression result
}

int lower_get_lvalue(Thir *lhs, Function *fn, Module *m) {
  switch (lhs->tag) {
  case THIR_MEMBER_ACCESS:
    return lower_member_rvalue(lhs, fn, m);
  case THIR_VARIABLE: {
    int tmp = generate_temp(fn);
    EMIT_LOAD(fn->code, tmp, (int)lhs->binding->index);
    return tmp;
  }
  default: {
    fprintf(stderr, "lower_get_lvalue_value: unsupported lvalue tag %d at %s", lhs->tag, lexer_span_to_string(lhs->span));
    exit(EXIT_FAILURE);
  }
  }
}

void lower_set_lvalue(Thir *lhs, int value, Function *fn) {
  switch (lhs->tag) {
  case THIR_MEMBER_ACCESS:
    (void)lower_member_assignment(lhs, value, fn);
    return;
  case THIR_VARIABLE:
    EMIT_STORE(fn->code, (int)lhs->binding->index, value);
    return;
  default: {
    fprintf(stderr, "lower_set_lvalue_value: unsupported lvalue tag %d at %s", lhs->tag, lexer_span_to_string(lhs->span));
    exit(EXIT_FAILURE);
  }
  }
}

int lower_lvalue_slot(Thir *n) {
  switch (n->tag) {
  case THIR_MEMBER_ACCESS:

  case THIR_VARIABLE:
    return (int)n->binding->index;
  default:
    fprintf(stderr, "lower_lvalue: unsupported lvalue tag %d at %s", n->tag, lexer_span_to_string(n->span));
    exit(EXIT_FAILURE);
  }
}

int lower_expression(Thir *n, Function *fn, Module *m) {
  assert(n && "null expression while lowering");
  switch (n->tag) {
  case THIR_ARRAY_INITIALIZER: {
    int dest = generate_temp(fn);
    unsigned long long length = n->array_initializer.values.length;
    EMIT_ALLOCA(fn->code, dest, n->type->pointee->index, length);
    LIST_FOREACH(n->array_initializer.values, value) {
      int v = lower_expression(value, fn, m);
      EMIT_MEMBER_STORE(fn->code, dest, __i, v);
    }
    return dest;
  } break;
  case THIR_MEMBER_ACCESS: {
    return lower_member_rvalue(n, fn, m);
  }
  case THIR_AGGREGATE_INITIALIZER: {
    int dest = generate_temp(fn);
    // length == 0 means one object, length > 0 means array for alloca
    EMIT_ALLOCA(fn->code, dest, n->type->index, 0);
    LIST_FOREACH(n->aggregate_initializer.values, value) {
      int v = lower_expression(value, fn, m);
      EMIT_MEMBER_STORE(fn->code, dest, __i, v);
    }
    return dest;
  }
  case THIR_LITERAL: {
    int dest = generate_temp(fn);
    unsigned cidx = add_constant(m, n);
    EMIT_CONST(fn->code, dest, (int)cidx);
    return dest;
  }
  case THIR_VARIABLE: {
    int dest = generate_temp(fn);
    size_t slot = n->binding->index;
    EMIT_LOAD(fn->code, dest, (int)slot);
    return dest;
  }
  case THIR_BINARY: {
    if (n->binary.op == OPERATOR_ASSIGN) {
      int rhs = lower_expression(n->binary.right, fn, m);
      lower_set_lvalue(n->binary.left, rhs, fn);
      return rhs;
    }

    if (operator_is_compound(n->binary.op)) {
      int lhs_val = lower_get_lvalue(n->binary.left, fn, m);
      int rhs = lower_expression(n->binary.right, fn, m);
      int res = generate_temp(fn);

      switch (n->binary.op) {
      case OPERATOR_PLUS_ASSIGN:
        EMIT_ADD(fn->code, res, lhs_val, rhs);
        break;
      case OPERATOR_MINUS_ASSIGN:
        EMIT_SUB(fn->code, res, lhs_val, rhs);
        break;
      case OPERATOR_STAR_ASSIGN:
        EMIT_MUL(fn->code, res, lhs_val, rhs);
        break;
      case OPERATOR_SLASH_ASSIGN:
        EMIT_DIV(fn->code, res, lhs_val, rhs);
        break;
      case OPERATOR_BIT_OR_ASSIGN:
        EMIT_BIT_OR(fn->code, res, lhs_val, rhs);
        break;
      case OPERATOR_BIT_AND_ASSIGN:
        EMIT_BIT_AND(fn->code, res, lhs_val, rhs);
        break;
      case OPERATOR_SHIFT_LEFT_ASSIGN:
        EMIT_SHIFT_LEFT(fn->code, res, lhs_val, rhs);
        break;
      case OPERATOR_SHIFT_RIGHT_ASSIGN:
        EMIT_SHIFT_RIGHT(fn->code, res, lhs_val, rhs);
        break;
      default:
        fprintf(stderr, "lower_expression: unhandled compound op %d\n", n->binary.op);
        exit(1);
      }

      lower_set_lvalue(n->binary.left, res, fn);
      return res;
    }

    int l = lower_expression(n->binary.left, fn, m);
    int r = lower_expression(n->binary.right, fn, m);
    int dest = generate_temp(fn);

    switch (n->binary.op) {
    case OPERATOR_MODULO:
      EMIT_MODULO(fn->code, dest, l, r);
      break;
    case OPERATOR_ADD:
      EMIT_ADD(fn->code, dest, l, r);
      break;
    case OPERATOR_SUB:
      EMIT_SUB(fn->code, dest, l, r);
      break;
    case OPERATOR_MUL:
      EMIT_MUL(fn->code, dest, l, r);
      break;
    case OPERATOR_DIV:
      EMIT_DIV(fn->code, dest, l, r);
      break;
    case OPERATOR_LOGICAL_OR:
      EMIT_LOGICAL_OR(fn->code, dest, l, r);
      break;
    case OPERATOR_LOGICAL_AND:
      EMIT_LOGICAL_AND(fn->code, dest, l, r);
      break;
    case OPERATOR_BIT_OR:
      EMIT_BIT_OR(fn->code, dest, l, r);
      break;
    case OPERATOR_BIT_AND:
      EMIT_BIT_AND(fn->code, dest, l, r);
      break;
    case OPERATOR_XOR:
      EMIT_XOR(fn->code, dest, l, r);
      break;
    case OPERATOR_EQUALS:
      EMIT_EQUALS(fn->code, dest, l, r);
      break;
    case OPERATOR_NOT_EQUALS:
      EMIT_NOT_EQUALS(fn->code, dest, l, r);
      break;
    case OPERATOR_LESS:
      EMIT_LESS(fn->code, dest, l, r);
      break;
    case OPERATOR_GREATER:
      EMIT_GREATER(fn->code, dest, l, r);
      break;
    case OPERATOR_LESS_EQUAL: {
      int tmp = generate_temp(fn);
      EMIT_GREATER(fn->code, tmp, l, r);
      EMIT_LOGICAL_NOT(fn->code, dest, tmp);
      break;
    }
    case OPERATOR_GREATER_EQUAL: {
      int tmp = generate_temp(fn);
      EMIT_LESS(fn->code, tmp, l, r);
      EMIT_LOGICAL_NOT(fn->code, dest, tmp);
      break;
    }
    case OPERATOR_SHIFT_LEFT:
      EMIT_SHIFT_LEFT(fn->code, dest, l, r);
      break;
    case OPERATOR_SHIFT_RIGHT:
      EMIT_SHIFT_RIGHT(fn->code, dest, l, r);
      break;
    case OPERATOR_INDEX: 
      EMIT_MEMBER_LOAD_INDIRECT(fn->code, dest, l, r);
      break;
    default:
      fprintf(stderr, "lower_expression: unhandled binary op %d\n", n->binary.op);
      break;
    }

    return dest;
  }
  case THIR_CALL: {
    int nargs = n->call.arguments.length;

    LIST_FOREACH(n->call.arguments, argument) {
      int dest = lower_expression(argument, fn, m);
      EMIT_PUSH(fn->code, dest);
    }

    int dest = generate_temp(fn);
    assert(n->call.callee && "null callee while lowering");

    if (n->call.callee->thir->tag == THIR_EXTERN) {
      EMIT_CALL_EXTERN(fn->code, dest, n->call.callee->thir->extern_function.index, nargs);
    } else {
      int func_idx = (int)n->call.callee->index;
      EMIT_CALL(fn->code, dest, func_idx, nargs);
    }
    return dest;
  }
  case THIR_UNARY: {
    int v = lower_expression(n->unary.operand, fn, m);
    int dest = generate_temp(fn);
    // We don't even have unary operators defined so nothing to do here
    EMIT_ADD(fn->code, dest, v, 0);
    return dest;
  }
  default:
    fprintf(stderr, "lower_expr: unhandled expression THIR tag %d\n", n->tag);
    exit(1);
  }
}

// Written by chatgpt cause im too stupid to do this
void lower_if(Thir *the_if, Function *fn, Module *m) {
  int cond = lower_expression(the_if->$if.condition, fn, m);

  // No else: need a guard jump to skip the then-block when cond is false.
  if (!the_if->$if.else_block) {
    int jif_idx = (int)fn->code.length;
    EMIT_JUMP_IF(fn->code, cond, 0); // jump to then_start if true

    int jmp_end_idx = (int)fn->code.length;
    EMIT_JUMP(fn->code, 0); // fall-through (false) jumps to end

    int then_start = (int)fn->code.length;
    fn->code.data[jif_idx].b = then_start - (jif_idx + 1); // patch to then

    lower_block(the_if->$if.then_block, fn, m);

    int end_ip = (int)fn->code.length;
    fn->code.data[jmp_end_idx].a = end_ip - (jmp_end_idx + 1); // patch to end
    return;
  }

  // With else:
  //   JUMP_IF cond -> then_start
  //   else...
  //   JUMP -> end
  //   then_start:
  //     then...
  //   end:
  int jif_idx = (int)fn->code.length;
  EMIT_JUMP_IF(fn->code, cond, 0); // patch to then-start

  if (the_if->$if.else_block->tag == THIR_IF) {
    lower_if(the_if->$if.else_block, fn, m);
  } else {
    lower_block(the_if->$if.else_block, fn, m);
  }

  int jmp_end_idx = (int)fn->code.length;
  EMIT_JUMP(fn->code, 0); // patch to end

  int then_start = (int)fn->code.length;
  fn->code.data[jif_idx].b = then_start - (jif_idx + 1); // to then

  lower_block(the_if->$if.then_block, fn, m);

  int end_ip = (int)fn->code.length;
  fn->code.data[jmp_end_idx].a = end_ip - (jmp_end_idx + 1); // to end
}

void lower_block(Thir *block, Function *fn, Module *m) {
  LIST_FOREACH(block->block, stmt) {
    switch (stmt->tag) {
    case THIR_IF: {
      lower_if(stmt, fn, m);
    } break;
    case THIR_RETURN: {
      if (stmt->return_value) {
        int tmp = lower_expression(stmt->return_value, fn, m);
        EMIT_RET(fn->code, tmp);
      } else {
        EMIT_RET(fn->code, -1);
      }
      break;
    }
    case THIR_VARIABLE: {
      int slot = (int)fn->n_locals++;
      stmt->binding->index = (size_t)slot;
      int src = lower_expression(stmt->variable_initializer, fn, m);
      EMIT_STORE(fn->code, slot, src);
      break;
    }
    case THIR_BINARY:
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
  assert(fnode && fnode->tag == THIR_FUNCTION && "lower_function: got non function node");
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
    EMIT_RET(fn->code, 0);
  }

  push_function(m, fn);
}

void lower_program(Thir *program, Module *m) {
  assert(program && program->tag == THIR_PROGRAM && "unexpected node while lowering, expected THIR_PROGRAM");

  LIST_INIT(m->functions);
  LIST_INIT(m->constants);

  LIST_FOREACH(program->program, f) {
    if (f->tag == THIR_EXTERN) {
      continue; // We just ignore it.
    } else {
      assert(f->tag == THIR_FUNCTION && "unexpected node type while lowering. expected THIR_FUNCTION");
      lower_function(f, m);
    }
  }
}

void print_instr(Instr *i, String_Builder *sb, int indent) {
  for (int j = 0; j < indent; ++j)
    sb_append(sb, "  ");
  switch (i->op) {
  case OP_ALLOCA:
    // ALLOCA t<dest>, <typeIndex> <length>
    sb_appendf(sb, "ALLOCA t%d, %d, %d", i->a, i->b, i->c);
    break;
  case OP_CONST:
    // CONST t<dest>, c<constIndex>
    sb_appendf(sb, "CONST t%d, c%d", i->a, i->b);
    break;
  case OP_LOAD:
    // LOAD t<dest>, <slot>
    sb_appendf(sb, "LOAD t%d, %d", i->a, i->b);
    break;
  case OP_STORE:
    // STORE <slot>, t<src>
    sb_appendf(sb, "STORE %d, t%d", i->a, i->b);
    break;
  case OP_MEMBER_LOAD:
    // MEMBER_LOAD t<dest>, t<obj>, <memberIndex>
    sb_appendf(sb, "MEMBER_LOAD t%d, t%d, %d", i->a, i->b, i->c);
    break;
  case OP_MEMBER_STORE:
    // MEMBER_STORE t<obj>, <memberIndex>, t<src>
    sb_appendf(sb, "MEMBER_STORE t%d, %d, t%d", i->a, i->b, i->c);
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
  case OP_MODULO:
    sb_appendf(sb, "MODULO t%d, t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_SHIFT_LEFT:
    sb_appendf(sb, "SHIFT_LEFT t%d, t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_SHIFT_RIGHT:
    sb_appendf(sb, "SHIFT_RIGHT t%d, t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_BIT_AND:
    sb_appendf(sb, "BIT_AND t%d, t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_BIT_OR:
    sb_appendf(sb, "BIT_OR t%d, t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_XOR:
    sb_appendf(sb, "XOR t%d, t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_EQUALS:
    sb_appendf(sb, "EQUALS t%d, t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_NOT_EQUALS:
    sb_appendf(sb, "NOT_EQUALS t%d, t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_LESS:
    sb_appendf(sb, "LESS t%d, t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_GREATER:
    sb_appendf(sb, "GREATER t%d, t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_LOGICAL_OR:
    sb_appendf(sb, "LOGICAL_OR t%d, t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_LOGICAL_AND:
    sb_appendf(sb, "LOGICAL_AND t%d, t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_NEGATE:
    // unary: NEGATE t<dest> t<src>
    sb_appendf(sb, "NEGATE t%d, t%d", i->a, i->b);
    break;
  case OP_LOGICAL_NOT:
    sb_appendf(sb, "LOGICAL_NOT t%d, t%d", i->a, i->b);
    break;
  case OP_BIT_NOT:
    sb_appendf(sb, "BIT_NOT t%d, t%d", i->a, i->b);
    break;
  case OP_PUSH:
    // PUSH t<src>
    sb_appendf(sb, "PUSH t%d", i->a);
    break;
  case OP_CALL:
    // CALL t<dest>, <funcIndex>, <nargs>
    sb_appendf(sb, "CALL t%d, %d, %d", i->a, i->b, i->c);
    break;
  case OP_CALL_EXTERN:
    // CALL_EXTERN t<dest>, <externIndex>, <nargs>
    sb_appendf(sb, "CALL_EXTERN t%d, %d, %d", i->a, i->b, i->c);
    break;
  case OP_RET:
    // RET t<src> | RET -1
    if (i->a >= 0)
      sb_appendf(sb, "RET t%d", i->a);
    else
      sb_append(sb, "RET -1");
    break;
  case OP_JUMP:
    // JUMP <offset>
    sb_appendf(sb, "JUMP %d", i->a);
    break;
  case OP_JUMP_IF:
    // JUMP_IF t<cond>, <offset>
    sb_appendf(sb, "JUMP_IF t%d, %d", i->a, i->b);
    break;
  default:
    sb_appendf(sb, "UNKNOWN %d", i->op);
    break;
  }
}

void print_module(Module *m, String_Builder *sb) {

  sb_append(sb, "EXTERNS:\n");
  for (size_t i = 0; i < CACHED_EXTERNS.length; ++i) {
    Extern_Function *ext = &CACHED_EXTERNS.data[i];
    sb_appendf(sb, "  [%zu]: extern %s(", i, ext->name);
    Function_Type *ftype = ext->function_type;
    for (size_t j = 0; j < ftype->parameters.length; ++j) {
      Type *param = ftype->parameters.data[j];
      print_type(param, sb);
      if (j + 1 < ftype->parameters.length)
        sb_append(sb, ", ");
    }
    sb_append(sb, ") -> ");
    print_type(ftype->returns, sb);
    sb_appendch(sb, '\n');
  }

  sb_append(sb, "TYPES:\n");
  for (size_t i = 0; i < m->types.length; ++i) {
    Type *type = m->types.data[i];
    if (type->tag == TYPE_STRUCT) {
      Struct_Type *struct_type = (Struct_Type *)type;
      sb_appendf(sb, "  [%zu]: struct %s {\n", i, type->name);
      LIST_FOREACH(struct_type->members, member) { sb_appendf(sb, "          [%zu]: %s\n", __i, member.type->name); }
      sb_append(sb, "        }\n");
    }
  }

  sb_append(sb, "CONSTANTS:\n");
  LIST_FOREACH(m->constants, constant) {
    sb_append(sb, "  ");
    switch (constant.type) {
    case CONST_TYPE_STRING:
      sb_appendf(sb, "[%d]: { type: %s, value: \"%s\\0\" }\n", __i, constant_type_to_string(constant.type), constant.value);
      break;
    case CONST_TYPE_INT:
      sb_appendf(sb, "[%d]: { type: %s, value: %s }\n", __i, constant_type_to_string(constant.type), constant.value);
      break;
    }
  }

  sb_append(sb, "FUNCTIONS:\n");
  LIST_FOREACH(m->functions, function) {
    sb_appendf(sb, "  %s:\n", function->name);
    for (unsigned i = 0; i < function->code.length; ++i) {
      print_instr(&function->code.data[i], sb, 2);
      sb_append(sb, "\n");
    }
  }
}

void module_init(Module *m, Context *context) { m->types = context->type_table; }