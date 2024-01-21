#include <stdio.h>
#include <stdlib.h>

#include "prt.h"

void __prt_trace();
void __prt_safe_trace();

void
__prt_assert(const char *cond, const char *file, int lineno, const char *func)
{
  fprintf(stderr, "%s:%d: %s: Assertion `%s' failed.\n", file, lineno, func, cond);
  __prt_trace();
  abort();
}

void
__prt_safe_assert(const char *cond, const char *file, int lineno, const char *func)
{
  fprintf(stderr, "%s:%d: %s: Assertion `%s' failed.\n", file, lineno, func, cond);
  __prt_safe_trace();
  abort();
}
