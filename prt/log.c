#include <stdio.h>

#include "prt.h"

/**
 * Implements `log`.
 *
 * @param s The NUL-terminated string to print to the standard output.
 */
void
__prt_log(prt_string_t s)
{
  PRT_ASSERT(s != NULL);
  puts(s);
}

/**
 * Implements `log (show b)` where `b` is a boolean.
 *
 * @param b The boolean to print to the standard output.
 */
void
__prt_log_bool(prt_bool_t b)
{
  if (b) {
    puts("true");
  } else {
    puts("false");
  }
}

/**
 * Implements `log (show v)` where `v` is an integer.
 *
 * @param v The integer to print to the standard output.
 */
void
__prt_log_int(prt_int_t v)
{
  printf("%lld", v);
}
