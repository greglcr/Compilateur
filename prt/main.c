#ifdef PRT_USE_GC
#include <gc.h>
#endif // PRT_USE_GC

// The main function generated by the compiler. The emitted symbol is mangled.
extern void _P4main();

int main()
{
#ifdef PRT_USE_GC
  GC_INIT();
#endif // PRT_USE_GC

  _P4main();
  return 0;
}
