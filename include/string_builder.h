#ifndef STRING_BUILDER_H
#define STRING_BUILDER_H

#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct String_Builder {
  unsigned int length;
  unsigned int capacity;
  char *value;
} String_Builder;

static inline void sb_append(String_Builder *sb, const char *str) {
  size_t str_len = strlen(str);
  while (sb->length + str_len >= sb->capacity) {
    sb->capacity = sb->capacity * 2 + str_len;
    sb->value = (char *)realloc(sb->value, sb->capacity);
  }
  memcpy(sb->value + sb->length, str, str_len);
  sb->length += str_len;
  sb->value[sb->length] = '\0';
}

static inline void sb_appendf(String_Builder *sb, const char *format, ...) {
  char buffer[4096];
  va_list args;
  va_start(args, format);
  vsnprintf(buffer, sizeof(buffer), format, args);
  va_end(args);
  sb_append(sb, buffer);
}

static inline char *sb_copy_string(const String_Builder *sb) {
  char *result = (char *)malloc(sb->length + 1);
  memcpy(result, sb->value, sb->length);
  result[sb->length] = '\0';
  return result;
}

static inline void sb_appendch(String_Builder *sb, char c) {
  if (sb->length + 1 >= sb->capacity) {
    sb->capacity = sb->capacity * 2 + 1;
    sb->value = (char *)realloc(sb->value, sb->capacity);
  }
  sb->value[sb->length++] = c;
  sb->value[sb->length] = '\0';
}

static inline void sb_free(String_Builder *sb) {
  free(sb->value);
  sb->capacity = 0;
  sb->length = 0;
}

#endif // #ifndef STRING_BUILDER_H