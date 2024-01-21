#include <stdio.h>

#include "prt.h"

/**
 * Implements `Show Boolean`.
 *
 * @param value The boolean value to be converted.
 * @return A statically allocated NUL-terminated string.
 */
prt_string_t
__prt_show_bool(prt_bool_t value)
{
  if (value)
  {
    return "true";
  }
  else
  {
    return "false";
  }
}

/**
 * Implements `Show Int`.
 *
 * @param value The integer value to be converted.
 * @return A dynamically GC-allocated NUL-terminated string.
 */
prt_string_t
__prt_show_int(prt_int_t value)
{
  size_t result_len = snprintf(NULL, 0, "%lld", value) + 1 /* NUL-terminated */;
  char *buffer = __prt_alloc(sizeof(char) * result_len);
  PRT_SAFE_ASSERT(buffer != NULL);
  snprintf(buffer, result_len, "%lld", value);
  return buffer;
}
