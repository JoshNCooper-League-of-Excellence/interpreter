extern abs :: (b int) int;
extern puts :: (s string) int;
extern printf :: (s string, x int) int;

print_something :: () void {
  var x = abs(100) * abs(0 - 100);
  printf("x=%d\n", x);
}

main :: (a string) int {
  print_something();
  return a + b;
}
