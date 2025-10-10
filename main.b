extern printf :: (string, int, int, int) int;

extern abs :: (int) int;

struct Vec3 {
  x int,
  y int,
  z int,
}

take_vec3 :: (v Vec3) void {
  printf("[in take_vec3]: %d, %d, %d\n", v.x, v.y, v.z); 
}

main :: (args string) void {
  v Vec3 = { 0, 0, 0 };
  x int = 0;
  x += 100;
  v.x += 100;
  take_vec3(v);
  value bool = 1 || 2;

  if false { 
    printf("[in main if]: %d, %d, %d\n", v.x, v.y, v.z);  
  } else if true {
    printf("[in main else if ]: %d, %d, %d\n", v.x, v.y, v.z);  
  } else {
    printf("[in main else]: %d, %d, %d\n", v.x, v.y, v.z);  
  }

}