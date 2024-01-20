# The MiniPureScript RunTime library

The MiniPureScript compiler needs, to implement some features, some builtins functions. These functions are implemented in this C library.

## Build

This project use Makefile. Simply type `make`. This will generate a `libprt.a` static library to be linked with the assembly code generated by the MiniPureScript compiler.

## Provided functions

All functions use the standard C calling function.

The runtime provides these functions:
- `__prt_alloc()`
- `__prt_alloc_atomic()`
- `__prt_strconcat()`
- `__prt_strcmp()`
- `__prt_show_int()`
- `__prt_show_bool()`
- `__prt_log()`
- `__prt_log_int()`
- `__prt_log_bool()`
- `__prt_div()`
- `__prt_rem()`

It also implements the C `main` function and calls `__ppurs_main` (which is the entry point of the compiled program by MiniPureScript).