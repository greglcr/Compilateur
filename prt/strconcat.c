#include <assert.h>
#include <string.h>

#include "prt.h"

/**
 * Implements string concatenation of the MiniPureScript language (the operator <>).
 *
 * The algorithm is linear in the size of both strings.
 *
 * @param lhs The left-hand side string, NUL-terminated.
 * @param rhs The right-hand side string, NUL-terminated.
 * @return A newly-allocated NUL-terminated string.
*/
prt_string_t
__prt_strconcat(prt_string_t lhs, prt_string_t rhs)
{
  assert(lhs != NULL && rhs != NULL);

  const size_t lhs_len = strlen(lhs);
  const size_t rhs_len = strlen(rhs);

  char* result = __prt_alloc(sizeof(char) * (lhs_len + rhs_len + 1));
  memcpy(result, lhs, lhs_len);
  memcpy(result + lhs_len, rhs, rhs_len);
  result[lhs_len + rhs_len] = '\0';
  return result;
}
