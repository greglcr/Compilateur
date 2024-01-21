#include <stdlib.h>
#include <execinfo.h>
#include <stdio.h>

/**
 * Prints the current backtrace to the standard error output.
 */
void
__prt_trace()
{
  void *array[10];

  int size = backtrace(array, 10);
  char **strings = backtrace_symbols(array, size);
  if (strings != NULL)
  {
    fputs("Backtrace:\n", stderr);
    for (int i = 0; i < size; i++)
      fprintf(stderr, "[%d]: %s\n", i, strings[i]);
  }

  free(strings);
}

/**
 * Same as __prt_trace() but do not malloc and therefore can be used in
 * scenarios where even malloc is failing (really bad error).
 *
 * However, some features from __prt_trace() may be missing.
 */
void
__prt_safe_trace()
{
  void *array[10];
  int size;

  size = backtrace(array, 10);
  fputs("Backtrace:\n", stderr);
  backtrace_symbols_fd(array, size, 2 /* stderr */);
}
