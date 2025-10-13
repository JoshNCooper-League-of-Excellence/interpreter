extern assert :: (int, byte*) void;
extern assert_eqi :: (int, int, byte*) void;
extern printf :: (byte*,int) void;

main :: () void {
  v int = 0;

  for i int = 0; i < 101; i += 1 {
    v = i;
  }

  assert(v == 100, "v != 100 (normal loop)");


  v = 0;
  for i int = 101; i > 0; i -= 1 {
    v = i;
  }

  assert(v == 1, "v != 1 (reverse loop)");


  v = 0;
  for i int = 0; i < 10; i += 1 {
    for j int = 0; j < i; j += 1 {
      v += j;
    }
  }
  assert(v == 120, "v != 120 (nested loop)");

  v = 0;
  for i int = 0; i < 10; i += 1 {
    if i == 5 {
      break;
    }
    v += 1;
  }
  assert_eqi(v, 5, "v != 5 (break in loop)");

  v = 0;
  for i int = 0; i < 10; i += 1 {
    if i % 2 == 0 {
      continue;
    }
    v += i;
  }
  assert(v == 25, "v != 25 (continue in loop)");
}