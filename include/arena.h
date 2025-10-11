#ifndef ARENA_H
#define ARENA_H

#include <stddef.h>
#include <stdlib.h>
typedef struct Arena {
  char *buf;
  size_t ptr;
  size_t capacity;
  struct Arena *next;
} Arena;

static inline void arena_init(Arena *arena, size_t capacity_in_bytes) {
  arena->capacity = capacity_in_bytes;
  arena->buf = calloc(capacity_in_bytes, sizeof(char));
  arena->next = nullptr;
}

static inline void arena_attach_next(Arena *arena) {
  Arena *new_arena = malloc(sizeof(Arena));
  arena_init(new_arena, arena->capacity);
  arena->next = new_arena;
}

static inline void *arena_alloc(Arena *arena, size_t size_in_bytes) {
  constexpr size_t MAX_ALIGN = alignof(max_align_t);

  size_t aligned_ptr = (arena->ptr + MAX_ALIGN - 1) & ~(MAX_ALIGN - 1);

  if (aligned_ptr + size_in_bytes > arena->capacity) {
    if (!arena->next) {
      arena_attach_next(arena);
    }
    return arena_alloc(arena->next, size_in_bytes);
  }

  char *mem = (char *)arena->buf + aligned_ptr;
  arena->ptr = aligned_ptr + size_in_bytes;
  return mem;
}

static inline void arena_free(Arena *arena) {
  if (arena->next) {
    arena_free(arena->next);
    free(arena->next);
  }
  free(arena->buf);
  arena->next = nullptr;
  arena->buf = nullptr;
  arena->capacity = 0;
  arena->ptr = 0;
}

#endif // #ifndef ARENA_H