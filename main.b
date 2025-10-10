extern puts :: (byte*) int;

extern strlen :: (byte*) int;

extern malloc :: (int) void*;

struct string {
  data byte*,
  length int
}

str :: (data byte*) string {
  return { data, strlen(data) };
}

main :: () void {
  x int = 0;
  s string = str("Hello, World!");

  if s.length > 12 {
    puts(s.data);
  }
}