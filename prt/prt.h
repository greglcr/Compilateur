#ifndef PRT_H
#define PRT_H

typedef long long prt_int_t;
typedef prt_int_t prt_bool_t;
typedef const char* prt_string_t;

void*
__prt_alloc(prt_int_t n);

#endif // !PRT_H
