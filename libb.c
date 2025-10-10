#include <assert.h>
#include <stdio.h>

/*
  This is just a scratch file for extern functions if you want any.
  Don't have to do anything but define functions (with normal C types)
  and use 'extern $name :: ($params) $return_type;' in the bi file.
*/
#include <stdbool.h>
#include <stdlib.h>

#undef assert

void assert(int value, const char *message) {
  if (!value) {
    fprintf(stderr, "assertion failed: '%s'\n", message);
    exit(1);
  }
}