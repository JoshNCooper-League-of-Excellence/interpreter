extern printf :: (msg string, v int, v int) int;

/* 
  struct Vec3 { 
    x int,  
    y int, 
    z int,
  };

  Vec3 vec3 = { 0, 0, 0 }; 
*/

main :: (args string) int {
  int x = 100;
  int y = x + 100;
  printf("Hello, World! x=%d, y=%d\n", x, y);
  return 0;
}
