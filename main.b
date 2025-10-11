/* 
  todo:
  
  [x] arena allocators 
  [/] typed operands for ir (*we have some type information, but needs to be more like LLVM's -- type encoded operands for every instr*)
  [] add modules and importing

  [] scope (non existent, we just use global bindings)
  [] fix ownership of alloca's: when we return a struct literal it gets destroyed on return.
  
  [] be able to manipulate unmanaged pointers with non-extern code
  [] be able to take non-array managed pointers

  [] improve ffi for structs, actual argument and return value marshalling

  [] either sized arrays or get rid of arrays
  [] string literals of 'string' type (fat pointer), not raw byte*

  easier and more extensible extern defs:
    [] add 'extern \"symbol_name\" from \"library_path\" our_fn_name :: (...) ...'; syntax
    [] add 'extern \"library name\" { ... group of functions ... }' syntax.
*/

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

str :: (data byte*) string {
  return { data, strlen(data) };
}

struct string { data byte*, length int }

extern printf :: (byte*, void*) int;
extern puts :: (byte*) int;
extern strlen :: (byte*) int;
extern malloc :: (int) void*;
extern free :: (void*) void;
extern memcpy :: (byte*, byte*, int) void;