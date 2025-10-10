
// extern printf :: (msg string, v int, v int) int;

extern printf :: (string, int, int, int) int;

struct Vec3 {
  x int,
  y int,
  z int,
}

main :: (args string) int {
  Vec3 vec3 = { 20, 30, 40 };
  vec3.x = 100;
  printf("%d, %d, %d\n", vec3.x, vec3.y, vec3.z);
  return 0;
}