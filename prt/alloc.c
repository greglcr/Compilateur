#include <assert.h>
#include <stdlib.h>

#include "prt.h"

#ifdef PRT_USE_GC
#include <gc.h>
#endif

/**
 * Allocates @n bytes of data and returns a pointer to it.
 *
 * The memory block is either allocated with the C function @c malloc
 * or with a GC allocator depending on the current configuration.
 *
 * @param n The count of bytes to allocate.
 * @return A pointer to the newly allocated block.
 */
void *
__prt_alloc(prt_int_t n)
{
  assert(n > 0); // we forbid 0 and negative values
#ifdef PRT_USE_GC
  return GC_MALLOC((size_t)n);
#else
  return malloc((size_t)n);
#endif // PRT_USE_GC
}

/**
 * Same as @c __prt_alloc except the allocated memory will never contain
 * any pointer.
 *
 * This is used by the GC allocator implementation to avoid searching
 * the allocated memory block for pointers. This is ideal for strings
 * or arrays of scalar types (integers, floating-points, etc.).
 */
void *
__prt_alloc_atomic(prt_int_t n)
{
  assert(n > 0); // we forbid 0 and negative values
#ifdef PRT_USE_GC
  return GC_MALLOC_ATOMIC((size_t)n);
#else
  return __prt_alloc(n);
#endif // PRT_USE_GC
}
