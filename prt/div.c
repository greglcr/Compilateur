#include "prt.h"

/**
 * Computes the quotient of the euclidean division of @a a by @a b.
 *
 * This is not exactly the same as the C native integer division.
 * See the PureScript Data.EuclideanRing documentation.
 *
 * @param a The dividend.
 * @param b The divisor.
 * @return The quotient.
 * @see https://pursuit.purescript.org/packages/purescript-prelude/5.0.1/docs/Data.EuclideanRing
 */
prt_int_t
__prt_div(prt_int_t a, prt_int_t b)
{
  if (b == 0)
  {
    // Division by 0 gives 0 by convention of PureScript.
    return 0;
  }
  else if (b < 0)
  {
    // The divisor is negative, we round towards positive infinity.
    return a / b + (a % b != 0 && (a <= 0));
  }
  else // if (b > 0)
  {
    // The divisor is positive, we round towards negative infinity.
    return a / b - (a % b != 0 && (a <= 0));
  }
}

/**
 * Computes the remainder of the euclidean division of @a a by @a b.
 *
 * This is not exactly the same as the C native integer division.
 * See the PureScript Data.EuclideanRing documentation.
 *
 * @param a The dividend.
 * @param b The divisor.
 * @return The remainder (always positive).
 * @see https://pursuit.purescript.org/packages/purescript-prelude/5.0.1/docs/Data.EuclideanRing
 */
prt_int_t
__prt_rem(prt_int_t a, prt_int_t b)
{
  // Division by 0 gives 0 by convention of PureScript.
  if (b == 0)
    return 0;

  // This test needed to prevent UB of `LLONG_MIN % -1`.
  if (b == -1)
    return 0;

  prt_int_t m = a % b;

  if (m < 0)
  {
    m = (b < 0) ? m - b : m + b;
  }

  return m;
}
