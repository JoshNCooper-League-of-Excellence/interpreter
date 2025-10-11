extern puts :: (byte*) int;
extern printf :: (byte*, void*) int;
extern strlen :: (byte*) int;
extern malloc :: (int) void*;
extern free :: (void*) void;
extern memcpy :: (byte*, byte*, int) void;

struct string { data byte*, length int }

str :: (data byte*) string {
  return { data, strlen(data) };
}

main :: () void {
  a int[] = {0, 100, 2};
  printf("a[1] = %d\n", a[1]);

  x int = 0;
  s string = str("Hello, World!");
  
  puts(s.data);

  p void* = malloc(1000);

  ab int[] = {0, 1, 2, 3, 4};

  printf("%p\n", p);
  memcpy(p, "Hello, World", 100);
  
  if s.length > 12 {
    puts(s.data);
  }

  free(p);
}