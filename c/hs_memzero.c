#include "hs_memzero.h"
#include <stdlib.h>

#if defined(HS_MEMZERO_DEBUG)
#  include <stdio.h>
#endif

#define HS_MEMZERO_IMPL_MEMSET_BARRIER 1
#define HS_MEMZERO_IMPL_EXPLICIT_BZERO 2
#define HS_MEMZERO_IMPL_SECURE_ZERO_MEMORY 3

#if !defined(HS_MEMZERO_IMPL)
#  if defined(_WIN32)
#    define HS_MEMZERO_IMPL HS_MEMZERO_IMPL_SECURE_ZERO_MEMORY
#    define HS_MEMZERO_IMPL_S "SECURE_ZERO_MEMORY"
#  elif defined(__linux__) || defined(__unix__) || (__STDC_VERSION__ >= 201112L)
#    define HS_MEMZERO_IMPL HS_MEMZERO_IMPL_EXPLICIT_BZERO
#    define HS_MEMZERO_IMPL_S "EXPLICIT_BZERO"
#  elif
#    define HS_MEMZERO_IMPL HS_MEMZERO_IMPL_MEMSET_BARRIER
#    define HS_MEMZERO_IMPL_S "MEMSET_BARRIER"
#  elif
#  endif
#endif

#if HS_MEMZERO_IMPL == HS_MEMZERO_IMPL_SECURE_ZERO_MEMORY
# include <Windows.h>
#elif (HS_MEMZERO_IMPL == HS_MEMZERO_IMPL_MEMSET_BARRIER) \
   || (HS_MEMZERO_IMPL == HS_MEMZERO_IMPL_EXPLICIT_BZERO)
# include <string.h>
#endif

#define HS_MEMZERO_WEAK_SYMBOLS (!defined(__ELF__) && !defined(__APPLE_CC__))
#if defined(HS_MEMZERO_WEAK_SYMBOLS)
__attribute__((weak))
void _hs_memzero_prevent_lto(void * const p, const size_t size);
__attribute__((weak))
void _hs_memzero_prevent_lto(void * const p, const size_t size) {
    (void) p; (void) size;
}
#endif

void hs_memzero(void * p, size_t size) {
#if defined(HS_MEMZERO_DEBUG)
  fprintf(stderr, "hs_memzero[%s](%p, %zu)\n", HS_MEMZERO_IMPL_S, p, size);
#endif

#if HS_MEMZERO_IMPL == HS_MEMZERO_IMPL_SECURE_ZERO_MEMORY
  SecureZeroMemory(p, size);

#elif HS_MEMZERO_IMPL == HS_MEMZERO_IMPL_EXPLICIT_BZERO
  explicit_bzero(p, size);

#elif HS_MEMZERO_IMPL == HS_MEMZERO_IMPL_MEMSET_BARRIER
  if (size > 0) {
    p = memset(p, 0, size);
#if defined(HS_MEMZERO_WEAK_SYMBOLS)
    _hs_memzero_prevent_lto(p, size);
#endif
    __asm__ __volatile__ ("" : : "r"(p) : "memory");
  }

#else
#  error Imposible HS_MEMZERO_IMPL
#endif
}

void hs_memzero_finalizerEnvFree(size_t * size, void * p) {
  hs_memzero(p, *size);
  free(size);
}

void hs_memzero_finalizerEnv(size_t * size, void * p) {
  hs_memzero(p, *size);
}
