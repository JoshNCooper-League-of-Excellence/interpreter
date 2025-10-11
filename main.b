/* 
  todo:
  [] scope
  [] typed operands for ir
  [] allocators 
  [] either sized arrays or get rid of arrays
  [] be able to manipulate unmanaged pointers with non-extern code
  [] be able to take non-array managed pointers
  [] string literals of string type, not byte*
  [] improve ffi for structs, actual marshalling  
*/


extern puts :: (byte*) int;
extern printf :: (byte*, void*) int;
extern strlen :: (byte*) int;
extern malloc :: (int) void*;
extern free :: (void*) void;
extern memcpy :: (byte*, byte*, int) void;

struct string { data byte*, length int }

str :: (data byte*) string {
  varyu byte* = "ehllow";
  return { data, strlen(data) };
}

main :: () void {
  a int[] = {0, 100, 2};
  printf("a[1] = %d\n", a[1]);

  a[0] = 10;
  printf("a[0] = %d\n", a[0]);

  x int = 0;
  s string = str("Hello, World!");

  ab int[] = {0, 1, 2, 3, 4};
  if s.length > 12 {
    puts(s.data);
  }
}