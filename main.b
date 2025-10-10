extern printf :: (string, int, int, int) int;

struct Vec3 {
  x int,
  y int,
  z int,
}

take_vec3 :: (v Vec3) void {
  printf("[in take_vec3]: %d, %d, %d\n", v.x, v.y, v.z); 
}

main :: (args string) int {
  Vec3 v = { 0, 0, 0 };
  take_vec3(v);
  printf("[in take_vec3]: %d, %d, %d\n", v.x, v.y, v.z); 
  return 0;
}