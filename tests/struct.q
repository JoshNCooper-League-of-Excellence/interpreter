main :: () void {
  vec Vec2 = { 0, 0 };
  assert(vec.x == vec.y && vec.y == 0, "zero initialized aggregate Vec2 failed");

  vec.x = 100;
  assert(vec.x == 100 && vec.y == 0, "setting struct Vec2's x failed");

  vec.y = vec.x * 2 - 10; 
  assert(vec.x == 100 && vec.y = 190, "complex member access failed");

  n NestedStruct = { vec, 10 };
  assert(n.z == 10 && n.xy.x == 100 && n.xy.y == 190, "nested struct failed");

  dn DoublyNestedStruct = { n, 100 };
  assert(dn.w == 100 && dn.n.z == 10 && dn.n.xy.x == 100 && dn.n.xy.y == 190, "doubly nested struct failed");

  vec_2 Vec2 = fn_returns_struct();
  assert(vec_2.x == 10 && vec_2.y == 10, "fn_return_struct() failed");

  assert(false, "todo(fix double free bug, which occurs at the exit of this block. see main.q todo's)");
}

fn_returns_struct :: () Vec2 {
  return { 10, 10 };
}

extern assert_eqi :: (int, int, byte*) void;
extern assert :: (int, byte*) void;

struct DoublyNestedStruct {
  n NestedStruct, 
  w int,
}

struct NestedStruct {
  xy Vec2,
  z int
}

struct Vec2 {
  x int,
  y int
}