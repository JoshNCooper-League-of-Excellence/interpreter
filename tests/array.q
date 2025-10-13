extern assert_eqi :: (int, int, byte*) void;
extern printf :: (byte*, int) void;

main :: () void {
  a int[] = {0, 1, 2, 3, 4};
  for i int = 0; i < 5; i += 1 {
    assert_eqi(a[i], i, "array initializer");
    a[i] = 10 * i; // prepare for next loop
  }
  for i int = 0; i < 5; i += 1 {
    assert_eqi(a[i], 10 * i, "mutated array failed");
  }
}