#include <stdio.h>
#include <stdlib.h>

#include "prt.h"

void
__prt_pattern_fail()
{
  fprintf(stderr, "Pattern matching failed.\n");
  __prt_trace();
  abort();
}
