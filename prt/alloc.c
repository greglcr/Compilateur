#include <assert.h>
#include <stdlib.h>

#include "prt.h"

#ifdef __PRT_USE_GC
#endif

void*
__prt_alloc(prt_int_t n)
{
  assert(n > 0); // we forbid 0 and negative values
#ifdef __PRT_USE_GC
  return gc_alloc((size_t)n);
#else
  return malloc((size_t)n);
#endif
}
