extern printf :: (msg string, v int, v int) int;


struct Vec3 { 
  x int,  
  y int, 
  z int,
}

main :: (args string) int {
  Vec3 vec3 = { 0, 0, 0 }; 

  printf("vec3.x = %d\n", vec3.x);

  int x = {100 * 2};
  int y = x + 100;
  printf("Hello, World! x=%d, y=%d\n", x, y);
  return 0;
}
