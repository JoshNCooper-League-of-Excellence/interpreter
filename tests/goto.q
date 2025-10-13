extern assert_eqi :: (int, int, byte*) void;

main :: () void {
  i int = 0;
  LABEL:
  if i < 10 {
    i += 1;
    goto LABEL;
  }

  assert_eqi(i, 10, "goto failed");
}