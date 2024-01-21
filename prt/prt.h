#ifndef PRT_H
#define PRT_H

typedef long long prt_int_t;
typedef prt_int_t prt_bool_t;
typedef const char *prt_string_t;

void *
__prt_alloc(prt_int_t n);
void *
__prt_alloc_atomic(prt_int_t n);

void __prt_assert(const char *cond, const char *file, int lineno, const char *func);
void __prt_safe_assert(const char *cond, const char *file, int lineno, const char *func);

#ifdef NDEBUG
#define PRT_ASSERT(cond)
#define PRT_SAFE_ASSERT(cond)
#else
#ifdef __GNUC__
#define PRT_ASSERT_FUNCNAME __extension__ __PRETTY_FUNCTION__
#else
#define PRT_ASSERT_FUNCNAME __func__
#endif

#define PRT_ASSERT_IMPL(func, expr, expr_txt) ((expr)          \
                                                   ? (void)(0) \
                                                   : func(expr_txt, __FILE__, __LINE__, PRT_ASSERT_FUNCNAME))
#define PRT_ASSERT(expr) PRT_ASSERT_IMPL(__prt_assert, expr, #expr)
#define PRT_SAFE_ASSERT(expr) PRT_ASSERT_IMPL(__prt_assert, expr, #expr)
#endif

#endif // !PRT_H
