/* 
  todo:

  [x] arena allocators 
  [x] scope (non existent, we just use global bindings)
  [/] typed operands for ir (*we have some type information, but needs to be more like LLVM's -- type encoded operands for every instr*)

  [] fix double free in Value when we make temps that are copies of structs.
    double free -- we don't really track _WHO_ is the original owned pointer of a struct,
    so when we create tempoaries that are copies of that struct for member access and such,
    all of them get freed, and obviously that is bad.

  [] fix ownership of alloca's: when we return a struct literal it gets destroyed on return.
  
  [] add modules and importing
  [] be able to manipulate unmanaged pointers with non-extern code
  [] be able to take non-array managed pointers

  [] improve ffi for structs, actual argument and return value marshalling

  [] either sized arrays or get rid of arrays
  [] string literals of 'string' type (fat pointer), not raw byte*

  easier to use and more extensible extern defs:
    [] add 'extern \"symbol_name\" from \"library_path\" our_fn_name :: (...) ...'; syntax
    [] add 'extern \"library name\" { ... group of functions ... }' syntax.
*/

main :: () void {
  a int[] = {0, 100, 2};
  printf("a[1] = %d\n", a[1]);

  a[0] = 10;
  printf("a[0] = %d\n", a[0]);
  s string = str("Hello, World!");

  x int = 0;
  puts("loop start\n");
  for i int = 0; i < 100000; i += 1 {
  }
  puts("loop end\n");

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