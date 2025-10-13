extern assert :: (int, byte*) void;
extern printf :: (byte*, int) void;

main :: () void {
  v int = 1;
  if false {
    assert(false, "if false -> hit");
  } else if false {
    assert(false, "if false/else if false -> hit");
  } else if false {
    assert(false, "if false/else if false/else if false -> hit");
  } else {
    v = 0;
  }

  assert(v == 0, "chained if false/else if false/... with else at the end failed");

  v = -1;
  if true {
    v = 0;
  } else if false {
    v = -1;
  } else if true {
    v = -2;
  }
  assert(v == 0, "chained if true/else if false/else if true failed");


  v = -1;
  if true {
    if false {
      assert(false, "if false in nested if failed");
    } else {
      v = 100;
    }
  }

  assert(v == 100, "nested if's failed");
}