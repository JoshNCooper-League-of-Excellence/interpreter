extern assert_eqi :: (int, int, byte*) void;

main :: () void {

  i int = 0;
  while {
    if i == 0 {
      i += 1;
      continue;
    }
    break;
    i += 1;
  }

  assert_eqi(i, 1, "while loop did not break");

}