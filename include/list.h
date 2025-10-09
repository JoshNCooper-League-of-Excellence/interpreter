#ifndef LIST_H
#define LIST_H

#define __VA_ARGS_EXPAND(macro, ...)                                           \
  __VA_ARGS_EXPAND_(__VA_ARGS__, __VA_ARGS_EXPAND_SEQ_N())(macro, __VA_ARGS__)

#define __VA_ARGS_EXPAND_(...) __VA_ARGS_EXPAND_N(__VA_ARGS__)
#define __VA_ARGS_EXPAND_N(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, N, ...)    \
  __VA_ARGS_EXPAND_##N

#define __VA_ARGS_EXPAND_SEQ_N() 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0
#define __VA_ARGS_EXPAND_1(macro, a1) macro(a1)
#define __VA_ARGS_EXPAND_2(macro, a1, a2) macro(a1) macro(a2)
#define __VA_ARGS_EXPAND_3(macro, a1, a2, a3) macro(a1) macro(a2) macro(a3)
#define __VA_ARGS_EXPAND_4(macro, a1, a2, a3, a4)                              \
  macro(a1) macro(a2) macro(a3) macro(a4)
#define __VA_ARGS_EXPAND_5(macro, a1, a2, a3, a4, a5)                          \
  macro(a1) macro(a2) macro(a3) macro(a4) macro(a5)
#define __VA_ARGS_EXPAND_6(macro, a1, a2, a3, a4, a5, a6)                      \
  macro(a1) macro(a2) macro(a3) macro(a4) macro(a5) macro(a6)
#define __VA_ARGS_EXPAND_7(macro, a1, a2, a3, a4, a5, a6, a7)                  \
  macro(a1) macro(a2) macro(a3) macro(a4) macro(a5) macro(a6) macro(a7)
#define __VA_ARGS_EXPAND_8(macro, a1, a2, a3, a4, a5, a6, a7, a8)              \
  macro(a1) macro(a2) macro(a3) macro(a4) macro(a5) macro(a6) macro(a7)        \
      macro(a8)
#define __VA_ARGS_EXPAND_9(macro, a1, a2, a3, a4, a5, a6, a7, a8, a9)          \
  macro(a1) macro(a2) macro(a3) macro(a4) macro(a5) macro(a6) macro(a7)        \
      macro(a8) macro(a9)
#define __VA_ARGS_EXPAND_10(macro, a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)    \
  macro(a1) macro(a2) macro(a3) macro(a4) macro(a5) macro(a6) macro(a7)        \
      macro(a8) macro(a9) macro(a10)

#include <string.h>

#define DEFINE_LISTS(...) __VA_ARGS_EXPAND(DEFINE_LIST, __VA_ARGS__)

#define DEFINE_LIST(T)                                                         \
  typedef struct T##_list {                                                    \
    T *data;                                                                   \
    unsigned int length;                                                       \
    unsigned int capacity;                                                     \
  } T##_list;

#define LIST_OF(T) T##_list

/* infer element size from list.data, avoid passing T for push/pop/free */
#define LIST_INIT(list)                                                        \
  do {                                                                         \
    (list).data = NULL;                                                        \
    (list).length = 0;                                                         \
    (list).capacity = 0;                                                       \
  } while (0)

#define LIST_PUSH(list, v)                                                     \
  do {                                                                         \
    if ((list).length == (list).capacity) {                                    \
      unsigned int newcap = (list).capacity ? (list).capacity * 4 : 1;         \
      void *tmp = realloc((list).data, newcap * sizeof *(list).data);          \
      if (!tmp)                                                                \
        break;                                                                 \
      (list).data = tmp;                                                       \
      (list).capacity = newcap;                                                \
    }                                                                          \
    (list).data[(list).length++] = (v);                                        \
  } while (0)

  #define LIST_PTR_PUSH(list, v)                                                 \
    do {                                                                         \
      if ((list)->length == (list)->capacity) {                                  \
        unsigned int newcap = (list)->capacity ? (list)->capacity * 4 : 1;       \
        void *tmp = realloc((list)->data, newcap * sizeof *((list)->data));      \
        if (!tmp)                                                               \
          break;                                                                \
        (list)->data = tmp;                                                     \
        (list)->capacity = newcap;                                              \
      }                                                                         \
      (list)->data[(list)->length++] = (v);                                     \
    } while (0)

/* LIST_POP uses GNU/Clang extensions (typeof + statement expression) to
 * synthesize a zero value */
#define LIST_POP(list)                                                         \
  ({                                                                           \
    typeof((list).data[0]) __ret = {0};                                        \
    if ((list).length > 0)                                                     \
      __ret = (list).data[--(list).length];                                    \
    __ret;                                                                     \
  })

#define LIST_REMOVE(list, idx)                                                 \
  do {                                                                         \
    if ((idx) < (list).length) {                                               \
      memmove(&(list).data[idx], &(list).data[(idx) + 1],                      \
              ((list).length - (idx) - 1) * sizeof *(list).data);              \
      --(list).length;                                                         \
    }                                                                          \
  } while (0)

#define LIST_FREE(list)                                                        \
  do {                                                                         \
    free((list).data);                                                         \
    (list).data = NULL;                                                        \
    (list).length = 0;                                                         \
    (list).capacity = 0;                                                       \
  } while (0)

#define LIST_CLEAR(list)                                                       \
  do {                                                                         \
    (list).length = 0;                                                         \
  } while (0)

#define LIST_FOREACH(list, var)                                                 \
  for (unsigned int __i = 0; __i < (list).length; ++__i)                        \
    for (int __once = 1; __once; __once = 0)                                    \
      for (typeof((list).data[0]) var = (list).data[__i]; __once; __once = 0)

#endif

