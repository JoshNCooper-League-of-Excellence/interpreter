extern assert :: (int, byte*) void;

main :: () void {
  a int = 10;
  b int = 3;

  assert(a + b == 13, "a + b == 13");
  assert(a - b == 7, "a - b == 7");
  assert(a * b == 30, "a * b == 30");
  assert(a / b == 3, "a / b == 3");

  assert(a % b == 1, "a % b == 1");

  assert((a & b) == 2, "a & b == 2");
  assert((a | b) == 11, "a | b == 11");
  assert((a ^ b) == 9, "a ^ b == 9");
  assert((a << b) == 80, "a << b == 80");
  assert((a >> b) == 1, "a >> b == 1");

  assert((a == b) == false, "a == b == false");
  assert((a == 10) == true, "a == 10 == true");

  assert((a != b) == true, "a != b == true");
  assert((a != 10) == false, "a != 10 == false");

  assert((a < b) == false, "a < b == false");
  assert((b < a) == true, "b < a == true");

  assert((a <= b) == false, "a <= b == false");
  assert((b <= a) == true, "b <= a == true");
  assert((a <= 10) == true, "a <= 10 == true");

  assert((a > b) == true, "a > b == true");
  assert((b > a) == false, "b > a == false");

  assert((a >= b) == true, "a >= b == true");
  assert((b >= a) == false, "b >= a == false");
  assert((a >= 10) == true, "a >= 10 == true");
}