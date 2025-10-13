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

void assert_eqi(int a, int b, const char *message) {
  if (a != b) {
    fprintf(stderr, "assertion failed: (%d != %d) '%s'\n", a, b, message);
    exit(1);
  }
}

void assert_eqp(void *a, void *b, const char *message) {
  if (a != b) {
    fprintf(stderr, "assertion failed: (%p != %p) '%s'\n", a, b, message);
    exit(1);
  }
}

void assert_eqs(const char *a, const char *b, const char *message) {
  if (a != b) {
    fprintf(stderr, "assertion failed: (%s != %s) '%s'\n", a, b, message);
    exit(1);
  }
}

typedef struct {
  int a, b;
} eqipair;

void assert_alltrue(eqipair *eqs, int len, const char *message) {
  for (int i = 0; i < len; ++i) {
    eqipair pair = eqs[i];
    if (pair.a != pair.b) {
      fprintf(stderr, "assertion failed: (%d != %d) '%s'\n", pair.a, pair.b, message);
      exit(1);
    }
  }
}