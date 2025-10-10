#ifndef CORE_H

#define CORE_H

#define TODO(msg)                                                              \
  do {                                                                         \
    fprintf(stderr, "%s\n", msg);                                              \
    exit(1);                                                                   \
  } while (0);

#endif