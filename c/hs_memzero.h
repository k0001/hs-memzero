#pragma once

#include <stddef.h>

// Zeroes `size` bytes starting at `p`.
void hs_memzero(void * p, size_t size);

// Zeroes `*size` bytes starting at `p`, and then `free()`s `size`.
void hs_memzero_finalizerEnvFree(size_t * size, void * p);

// Zeroes `*size` bytes starting at `p`.
void hs_memzero_finalizerEnv(size_t * size, void * p);

