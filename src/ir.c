#include "ir.h"
#include "ast.h"
#include "binding.h"
#include "lexer.h"
#include "list.h"
#include "string_builder.h"
#include "thir.h"
#include "type.h"

/**
 * IR quick reference (for VM/compiler implementers)
 *
 * Value model
 * - integer: signed 64-bit ("integer"). Booleans are unsignedegers 0/1.
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
 * - RET: a=srcTemp or INT_MAX for void.
 *
 * Arithmetic and logic (all unsignedeger semantics)
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
    assert(false && "[IR]: invalid constant type, expected 'unsigned' or 'string'");
    break;
  }

  const char *value = thir->literal.value;

  if (is_bool) {
    if (strncmp("true", value, 4) == 0) {
      value = "1";
    } else if (strncmp("false", value, 5) == 0) {
      value = "0";
    } else {
      assert(false && "[IR]: invalid constant bool, expected 'true' or 'false'");
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

unsigned generate_temp(Function *fn) {
  unsigned dest = fn->n_locals;
  fn->n_locals += 1;
  return dest;
}

int collect_member_path(Thir *n, int leaf_to_root[], int max_depth, Thir **out_base) {
  int depth = 0;
  Thir *cur = n;
  while (cur->tag == THIR_MEMBER_ACCESS) {
    assert(depth < max_depth && "member access chain too deep");
    leaf_to_root[depth++] = (unsigned)cur->member_access.index;
    cur = cur->member_access.base;
  }
  *out_base = cur;
  return depth;
}

int lower_member_rvalue(Thir *n, Function *fn, Module *m) {
  int leaf_to_root[32] = {0};
  Thir *base = NULL;
  int depth = collect_member_path(n, leaf_to_root, 32, &base);

  int obj;
  if (base->tag == THIR_VARIABLE) {
    obj = generate_temp(fn);
    EMIT_LOAD(fn->code, obj, base->binding->index);
  } else {
    obj = lower_expression(base, fn, m);
  }

  for (int i = depth - 1; i >= 0; --i) {
    unsigned tmp = generate_temp(fn);
    EMIT_MEMBER_LOAD(fn->code, tmp, obj, leaf_to_root[i]);
    obj = tmp;
  }
  return obj;
}

// n is a binary INDEX node
unsigned lower_index_rvalue(Thir *n, Function *fn, Module *m) {
  Thir *base = n->binary.left;

  unsigned obj;
  if (base->tag == THIR_VARIABLE) {
    obj = generate_temp(fn);
    EMIT_LOAD(fn->code, obj, (unsigned)base->binding->index);
  } else {
    obj = lower_expression(base, fn, m);
  }

  unsigned idx = lower_expression(n->binary.right, fn, m);
  unsigned out = generate_temp(fn);
  EMIT_MEMBER_LOAD_INDIRECT(fn->code, out, obj, idx);
  return out;
}

// arr[i] = rhs (root must be a variable)
unsigned lower_index_assignment(Thir *lhs, unsigned rhs, Function *fn, Module *m) {
  Thir *base = lhs->binary.left;
  assert(base->tag == THIR_VARIABLE && "assignment to non-lvalue index base");

  unsigned slot = (unsigned)base->binding->index;
  unsigned obj = generate_temp(fn);
  EMIT_LOAD(fn->code, obj, slot);

  unsigned idx = lower_expression(lhs->binary.right, fn, m);
  EMIT_MEMBER_STORE_INDIRECT(fn->code, obj, idx, rhs);

  EMIT_STORE(fn->code, slot, obj);
  return rhs;
}

unsigned lower_member_assignment(Thir *lhs, unsigned rhs, Function *fn) {
  int leaf_to_root[32];
  Thir *base = NULL;
  int depth = collect_member_path(lhs, leaf_to_root, 32, &base);
  assert(depth > 0 && "expected member chain for member assignment");

  // Root-to-leaf order
  unsigned path[32];
  for (int i = 0; i < depth; ++i)
    path[i] = leaf_to_root[depth - 1 - i];

  // Only variables are assignable as the root container for now.
  assert(base->tag == THIR_VARIABLE && "assignment to non-lvalue member base");

  unsigned slot = (unsigned)base->binding->index;

  // Load the top-level struct value
  unsigned obj0 = generate_temp(fn);
  EMIT_LOAD(fn->code, obj0, slot);

  // Build containers down to the parent of the leaf
  unsigned containers[33];
  containers[0] = obj0;
  for (int i = 0; i < depth - 1; ++i) {
    unsigned tmp = generate_temp(fn);
    EMIT_MEMBER_LOAD(fn->code, tmp, containers[i], path[i]);
    containers[i + 1] = tmp;
  }

  // Write the rhs unsignedo the leaf field
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

unsigned lower_get_lvalue(Thir *lhs, Function *fn, Module *m) {
  switch (lhs->tag) {
  case THIR_MEMBER_ACCESS:
    return lower_member_rvalue(lhs, fn, m);
  case THIR_BINARY:
    if (lhs->binary.op == OPERATOR_INDEX) {
      return lower_index_rvalue(lhs, fn, m);
    }
    break;
  case THIR_VARIABLE: {
    unsigned tmp = generate_temp(fn);
    EMIT_LOAD(fn->code, tmp, (unsigned)lhs->binding->index);
    return tmp;
  }
  default: {
    fprintf(stderr, "lower_get_lvalue_value: unsupported lvalue tag %d at %s", lhs->tag, lexer_span_to_string(lhs->span));
    exit(EXIT_FAILURE);
  }
  }
  return -1;
}

void lower_set_lvalue(Thir *lhs, unsigned value, Function *fn, Module *m) {
  switch (lhs->tag) {
  case THIR_MEMBER_ACCESS:
    (void)lower_member_assignment(lhs, value, fn);
    return;
  case THIR_BINARY:
    if (lhs->binary.op == OPERATOR_INDEX) {
      (void)lower_index_assignment(lhs, value, fn, m);
      return;
    }
    break;
  case THIR_VARIABLE:
    EMIT_STORE(fn->code, (unsigned)lhs->binding->index, value);
    return;
  default: {
    fprintf(stderr, "lower_set_lvalue_value: unsupported lvalue tag %d at %s", lhs->tag, lexer_span_to_string(lhs->span));
    exit(EXIT_FAILURE);
  }
  }
}

unsigned lower_lvalue_slot(Thir *n) {
  switch (n->tag) {
  case THIR_MEMBER_ACCESS:

  case THIR_VARIABLE:
    return (unsigned)n->binding->index;
  default:
    fprintf(stderr, "lower_lvalue: unsupported lvalue tag %d at %s", n->tag, lexer_span_to_string(n->span));
    exit(EXIT_FAILURE);
  }
}

unsigned lower_expression(Thir *n, Function *fn, Module *m) {
  assert(n && "null expression while lowering");
  switch (n->tag) {
  case THIR_ARRAY_INITIALIZER: {
    unsigned dest = generate_temp(fn);
    unsigned long long length = n->array_initializer.values.length;
    EMIT_ALLOCA(fn->code, dest, n->type->pointee->index, length);
    LIST_FOREACH(n->array_initializer.values, value) {
      unsigned v = lower_expression(value, fn, m);
      EMIT_MEMBER_STORE(fn->code, dest, __i, v);
    }
    return dest;
  }
  case THIR_MEMBER_ACCESS: {
    return lower_member_rvalue(n, fn, m);
  }
  case THIR_AGGREGATE_INITIALIZER: {
    unsigned dest = generate_temp(fn);
    // length == 0 means one object, length > 0 means array for alloca
    EMIT_ALLOCA(fn->code, dest, n->type->index, 0);
    LIST_FOREACH(n->aggregate_initializer.values, value) {
      unsigned v = lower_expression(value, fn, m);
      EMIT_MEMBER_STORE(fn->code, dest, __i, v);
    }
    return dest;
  }
  case THIR_LITERAL: {
    unsigned dest = generate_temp(fn);
    unsigned cidx = add_constant(m, n);
    EMIT_CONST(fn->code, dest, (unsigned)cidx);
    return dest;
  }
  case THIR_VARIABLE: {
    unsigned dest = generate_temp(fn);
    size_t slot = n->binding->index;
    EMIT_LOAD(fn->code, dest, (unsigned)slot);
    return dest;
  }
  case THIR_BINARY: {
    if (n->binary.op == OPERATOR_ASSIGN) {
      unsigned rhs = lower_expression(n->binary.right, fn, m);
      lower_set_lvalue(n->binary.left, rhs, fn, m);
      return rhs;
    }

    if (operator_is_compound(n->binary.op)) {
      unsigned lhs_val = lower_get_lvalue(n->binary.left, fn, m);
      unsigned rhs = lower_expression(n->binary.right, fn, m);
      unsigned res = generate_temp(fn);

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

      lower_set_lvalue(n->binary.left, res, fn, m);
      return res;
    }

    unsigned l = lower_expression(n->binary.left, fn, m);
    unsigned r = lower_expression(n->binary.right, fn, m);
    unsigned dest = generate_temp(fn);

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
      unsigned tmp = generate_temp(fn);
      EMIT_GREATER(fn->code, tmp, l, r);
      EMIT_LOGICAL_NOT(fn->code, dest, tmp);
    } break;
    case OPERATOR_GREATER_EQUAL: {
      unsigned tmp = generate_temp(fn);
      EMIT_LESS(fn->code, tmp, l, r);
      EMIT_LOGICAL_NOT(fn->code, dest, tmp);
    } break;
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
    }

    return dest;
  }
  case THIR_CALL: {
    unsigned nargs = n->call.arguments.length;
    LIST_FOREACH(n->call.arguments, argument) {
      unsigned dest = lower_expression(argument, fn, m);
      EMIT_PUSH(fn->code, dest);
    }
    assert(n->call.callee && "null callee while lowering");
    unsigned dest = generate_temp(fn);
    if (n->call.callee->thir->tag == THIR_EXTERN) {
      EMIT_CALL_EXTERN(fn->code, dest, n->call.callee->thir->extern_function.index, nargs);
    } else {
      unsigned func_idx = (unsigned)n->call.callee->index;
      EMIT_CALL(fn->code, dest, func_idx, nargs);
    }
    return dest;
  }
  case THIR_UNARY: {
    unsigned v = lower_expression(n->unary.operand, fn, m);
    unsigned dest = generate_temp(fn);
    // We don't even have unary operators defined so nothing to do here
    EMIT_ADD(fn->code, dest, v, 0);
    return dest;
  }
  default:
    fprintf(stderr, "lower_expr: unhandled expression THIR tag %d\n", n->tag);
    exit(1);
    break;
  }
}

void lower_if(Thir *the_if, Function *fn, Module *m, IR_Context *c) {
  unsigned cond = lower_expression(the_if->$if.condition, fn, m);

  // No else: need a guard jump to skip the then-block when cond is false.
  if (!the_if->$if.else_block) {
    unsigned jif_idx = (unsigned)fn->code.length;
    EMIT_JUMP_IF(fn->code, cond, 0); // jump to then_start if true

    unsigned jmp_end_idx = (unsigned)fn->code.length;
    EMIT_JUMP(fn->code, 0); // fall-through (false) jumps to end

    unsigned then_start = (unsigned)fn->code.length;
    fn->code.data[jif_idx].b = then_start - (jif_idx + 1); // patch to then

    lower_block(the_if->$if.then_block, fn, m, c);

    unsigned end_ip = (unsigned)fn->code.length;
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
  unsigned jif_idx = (unsigned)fn->code.length;
  EMIT_JUMP_IF(fn->code, cond, 0); // patch to then-start

  if (the_if->$if.else_block->tag == THIR_IF) {
    lower_if(the_if->$if.else_block, fn, m, c);
  } else {
    lower_block(the_if->$if.else_block, fn, m, c);
  }

  unsigned jmp_end_idx = (unsigned)fn->code.length;
  EMIT_JUMP(fn->code, 0); // patch to end

  unsigned then_start = (unsigned)fn->code.length;
  fn->code.data[jif_idx].b = then_start - (jif_idx + 1); // to then

  lower_block(the_if->$if.then_block, fn, m, c);

  unsigned end_ip = (unsigned)fn->code.length;
  fn->code.data[jmp_end_idx].a = end_ip - (jmp_end_idx + 1); // to end
}

void lower_variable(Thir *stmt, Function *fn, Module *m) {
  unsigned slot = generate_temp(fn);
  stmt->binding->index = slot;
  unsigned src = lower_expression(stmt->variable_initializer, fn, m);
  EMIT_STORE(fn->code, slot, src);
}

void lower_control_flow_change(Thir *stmt, Function *fn, IR_Context *c) {
  switch (stmt->control_flow_change.tag) {
  case CF_BREAK:
    c->break_patches[(*c->break_patches_length)++] = fn->code.length;
    EMIT_JUMP(fn->code, 0);
    break;
  case CF_CONTINUE:
    c->cont_patches[(*c->cont_patches_length)++] = fn->code.length;
    EMIT_JUMP(fn->code, 0);
    break;
  case CF_GOTO:
    Goto $goto = {
        .fn = fn,
        .offset = fn->code.length,
        .target_label = stmt->control_flow_change.target_label,
    };
    // resolved later, after we've collected all the labels.
    LIST_PUSH(c->gotos, $goto);
    EMIT_JUMP(fn->code, 0);
    break;
  }
}

void lower_loop(Thir *stmt, Function *fn, Module *m, IR_Context *c) {
  if (stmt->loop.init) {
    lower_variable(stmt->loop.init, fn, m);
  }

  unsigned loop_start = fn->code.length;
  unsigned cond = lower_expression(stmt->loop.condition, fn, m);
  unsigned jif_idx = fn->code.length;
  EMIT_JUMP_IF(fn->code, cond, 0); // jump to body if true

  unsigned jmp_end_idx = fn->code.length;
  EMIT_JUMP(fn->code, 0); // jump to end if false

  unsigned body_start = fn->code.length;

  // clang-format off
  unsigned *old_breaks = c->break_patches, 
           *old_conts = c->cont_patches, 
            *old_breaks_length = c->break_patches_length,
            *old_conts_length = c->cont_patches_length;
  // clang-format on

  // TODO: this shouldn't be fixed. should use a unsigned_list struct { unsigned *data,
  // length, capacity; } list; with list library

  unsigned break_patch_list[256];
  unsigned cont_patch_list[256];
  unsigned break_count = 0, cont_count = 0;

  c->break_patches = break_patch_list;
  c->cont_patches = cont_patch_list;
  c->cont_patches_length = &cont_count;
  c->break_patches_length = &break_count;

  lower_block(stmt->loop.block, fn, m, c);

  unsigned update_start = fn->code.length;
  if (stmt->loop.update) {
    lower_expression(stmt->loop.update, fn, m);
  }

  EMIT_JUMP(fn->code, loop_start - fn->code.length); // jump back to loop start

  unsigned end_ip = fn->code.length;

  for (unsigned i = 0; i < cont_count; ++i) {
    unsigned patch_idx = cont_patch_list[i];
    fn->code.data[patch_idx].a = update_start - (patch_idx + 1);
  }

  for (unsigned i = 0; i < break_count; ++i) {
    unsigned patch_idx = break_patch_list[i];
    fn->code.data[patch_idx].a = end_ip - (patch_idx + 1);
  }

  fn->code.data[jif_idx].b = body_start - (jif_idx + 1);     // patch JUMP_IF to body
  fn->code.data[jmp_end_idx].a = end_ip - (jmp_end_idx + 1); // patch JUMP to end

  // reset previous state.
  c->break_patches = old_breaks;
  c->break_patches_length = old_breaks_length;
  c->cont_patches = old_conts;
  c->cont_patches_length = old_conts_length;
}

void lower_stmt(Thir *stmt, Function *fn, Module *m, IR_Context *c) {
  switch (stmt->tag) {
  case THIR_LABEL: {
    Label label = {
      .owner = fn,
      .name = stmt->label.name,
      .offset = fn->code.length,
    };
    LIST_PUSH(c->labels, label);
    break;
  }
  case THIR_CONTROL_FLOW_CHANGE:
    lower_control_flow_change(stmt, fn, c);
    break;
  case THIR_LOOP: {
    lower_loop(stmt, fn, m, c);
  } break;
  case THIR_IF: {
    lower_if(stmt, fn, m, c);
  } break;
  case THIR_RETURN: {
    if (stmt->return_value) {
      unsigned tmp = lower_expression(stmt->return_value, fn, m);
      EMIT_RET(fn->code, tmp);
    } else {
      EMIT_RET(fn->code, INT_MAX);
    }
  } break;
  case THIR_VARIABLE: {
    lower_variable(stmt, fn, m);
  } break;
  case THIR_BINARY:
  case THIR_CALL:
  case THIR_LITERAL:
    lower_expression(stmt, fn, m);
    break;
  default: {
    assert(false && "unexpected THIR node in block while lowering");
  } break;
  }
}

void lower_block(Thir *block, Function *fn, Module *m, IR_Context *c) {
  LIST_FOREACH(block->block, stmt) { lower_stmt(stmt, fn, m, c); }
}

void lower_function(Thir *fnode, Module *m, IR_Context *c) {
  assert(fnode && fnode->tag == THIR_FUNCTION && "lower_function: got non function node");
  Function *fn = calloc(1, sizeof(Function));
  fn->type = (Function_Type *)fnode->type;
  fn->name = fnode->function.name;
  fn->n_locals = 0;
  fn->param_count = fnode->function.parameters.length;
  fn->const_start = m->constants.length;

  if (strcmp(fn->name, "main") == 0) {
    m->entry_point = fn;
  }

  for (unsigned i = 0; i < fnode->function.parameters.length; ++i) {
    Binding *b = fnode->function.parameters.data[i];
    assert(b && "got invalid binding in parameter list while lowering");
    b->index = i;
  }

  for (unsigned i = 0; i < fnode->function.parameters.length; ++i) {
    generate_temp(fn);
  }

  lower_block(fnode->function.block, fn, m, c);

  // ensure we always return
  if (fn->code.length == 0 || fn->code.data[fn->code.length - 1].op != OP_RET) {
    EMIT_RET(fn->code, INT_MAX);
  }

  push_function(m, fn);
}

void lower_program(Thir *program, Module *m, IR_Context *c) {
  assert(program && program->tag == THIR_PROGRAM && "unexpected node while lowering, expected THIR_PROGRAM");

  LIST_INIT(m->functions);
  LIST_INIT(m->constants);

  LIST_FOREACH(program->program, f) {
    if (f->tag == THIR_EXTERN) {
      continue; // We just ignore it.
    } else {
      assert(f->tag == THIR_FUNCTION && "unexpected node type while lowering. expected THIR_FUNCTION");
      lower_function(f, m, c);
    }
  }

  LIST_FOREACH(c->gotos, $goto) {
    Label *label = nullptr;

    for (unsigned i = 0; i < c->labels.length; ++i) {
      Label *search = &c->labels.data[i];
      if (strcmp($goto.target_label, search->name) == 0 && $goto.fn == search->owner) {
        label = &c->labels.data[i];
        break;
      }
    }

    if (!label) {
      fprintf(stderr, "[IR]: unresolved label '%s'\n", $goto.target_label);
      exit(EXIT_FAILURE);
    }
    signed dest = label->offset;
    $goto.fn->code.data[$goto.offset].a = (dest - $goto.offset) - 1;
  }
}

void print_instr(Instr *i, String_Builder *sb, unsigned indent) {
  for (unsigned j = 0; j < indent; ++j) {
    sb_append(sb, " ");
  }
  switch (i->op) {
  case OP_ALLOCA:
    sb_appendf(sb, "t%d = ALLOCA %d, %d", i->a, i->b, i->c);
    break;
  case OP_CONST:
    sb_appendf(sb, "t%d = CONST c%d", i->a, i->b);
    break;
  case OP_LOAD:
    sb_appendf(sb, "t%d = LOAD %d", i->a, i->b);
    break;
  case OP_MEMBER_LOAD:
    sb_appendf(sb, "t%d = MEMBER_LOAD t%d, %d", i->a, i->b, i->c);
    break;
  case OP_ADD:
    sb_appendf(sb, "t%d = ADD t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_SUB:
    sb_appendf(sb, "t%d = SUB t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_MUL:
    sb_appendf(sb, "t%d = MUL t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_DIV:
    sb_appendf(sb, "t%d = DIV t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_MODULO:
    sb_appendf(sb, "t%d = MODULO t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_SHIFT_LEFT:
    sb_appendf(sb, "t%d = SHIFT_LEFT t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_SHIFT_RIGHT:
    sb_appendf(sb, "t%d = SHIFT_RIGHT t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_BIT_AND:
    sb_appendf(sb, "t%d = BIT_AND t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_BIT_OR:
    sb_appendf(sb, "t%d = BIT_OR t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_XOR:
    sb_appendf(sb, "t%d = XOR t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_EQUALS:
    sb_appendf(sb, "t%d = EQUALS t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_NOT_EQUALS:
    sb_appendf(sb, "t%d = NOT_EQUALS t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_LESS:
    sb_appendf(sb, "t%d = LESS t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_GREATER:
    sb_appendf(sb, "t%d = GREATER t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_LOGICAL_OR:
    sb_appendf(sb, "t%d = LOGICAL_OR t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_LOGICAL_AND:
    sb_appendf(sb, "t%d = LOGICAL_AND t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_NEGATE:
    sb_appendf(sb, "t%d = NEGATE t%d", i->a, i->b);
    break;
  case OP_LOGICAL_NOT:
    sb_appendf(sb, "t%d = LOGICAL_NOT t%d", i->a, i->b);
    break;
  case OP_BIT_NOT:
    sb_appendf(sb, "t%d = BIT_NOT t%d", i->a, i->b);
    break;
  case OP_CALL:
    sb_appendf(sb, "t%d = CALL %d, %d", i->a, i->b, i->c);
    break;
  case OP_CALL_EXTERN:
    sb_appendf(sb, "t%d = CALL_EXTERN %d, %d", i->a, i->b, i->c);
    break;
  case OP_STORE:
    sb_appendf(sb, "STORE %d, t%d", i->a, i->b);
    break;
  case OP_MEMBER_STORE:
    sb_appendf(sb, "MEMBER_STORE t%d, %d, t%d", i->a, i->b, i->c);
    break;
  case OP_PUSH:
    sb_appendf(sb, "PUSH t%d", i->a);
    break;
  case OP_RET:
    if (i->a >= 0)
      sb_appendf(sb, "RET t%d", i->a);
    else
      sb_append(sb, "RET -1");
    break;
  case OP_JUMP:
    sb_appendf(sb, "JUMP %d", i->a);
    break;
  case OP_JUMP_IF:
    sb_appendf(sb, "JUMP_IF t%d, %d", i->a, i->b);
    break;
  case OP_MEMBER_LOAD_INDIRECT:
    sb_appendf(sb, "t%d = MEMBER_LOAD_INDIRECT t%d, t%d", i->a, i->b, i->c);
    break;
  case OP_MEMBER_STORE_INDIRECT:
    sb_appendf(sb, "MEMBER_STORE_INDIRECT t%d, t%d, t%d", i->a, i->b, i->c);
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
    sb_append(sb, ") ");
    print_type(ftype->returns, sb);
    sb_appendch(sb, '\n');
  }

  sb_append(sb, "TYPES:\n");
  for (size_t i = 0; i < m->types.length; ++i) {
    Type *type = m->types.data[i];

    switch (type->tag) {
    case TYPE_FUNCTION:
      break; // ignored.
    case TYPE_INT:
    case TYPE_BYTE:
    case TYPE_VOID:
    case TYPE_BOOL:
      sb_appendf(sb, "  [%zu]: ", i);
      print_type(type, sb);
      sb_appendch(sb, '\n');
      break;
    case TYPE_STRUCT:
      Struct_Type *struct_type = (Struct_Type *)type;
      sb_appendf(sb, "  [%zu]: struct %s {\n", i, type->name);
      LIST_FOREACH(struct_type->members, member) { sb_appendf(sb, "          [%zu]: %s\n", __i, member.type->name); }
      sb_append(sb, "        }\n");
    }
    break;
  }

  sb_append(sb, "CONSTANTS:\n");
  LIST_FOREACH(m->constants, constant) {
    sb_append(sb, "  ");
    switch (constant.type) {
    case CONST_TYPE_STRING:
      sb_appendf(sb, "[%d]: %s :: \"%s\\0\"\n", __i, constant_type_to_string(constant.type), constant.value);
      break;
    case CONST_TYPE_INT:
      sb_appendf(sb, "[%d]: %s :: %s\n", __i, constant_type_to_string(constant.type), constant.value);
    }
    break;
  }

  sb_append(sb, "FUNCTIONS:\n");
  LIST_FOREACH(m->functions, function) {
    sb_appendf(sb, "%s(", function->name);
    for (unsigned i = 0; i < function->param_count; ++i) {
      Type *type = function->type->parameters.data[i];
      sb_appendf(sb, "%d ", i);
      print_type(type, sb);
      if (i != function->param_count - 1) {
        sb_append(sb, ", ");
      }
    }
    sb_append(sb, ") ");
    print_type(function->type->returns, sb);
    sb_append(sb, ":\n");
    for (unsigned i = 0; i < function->code.length; ++i) {
      print_instr(&function->code.data[i], sb, 2);
      sb_append(sb, "\n");
    }
  }
}

void module_init(Module *m, Context *context) { m->types = context->type_table; }