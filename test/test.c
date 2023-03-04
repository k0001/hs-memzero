#include <hs_memzero.h>
#include <stdlib.h>
#include <stdio.h>

void fill(void * p, size_t size) {
  for (size_t i=0; i < size; i++) {
    ((unsigned char *)p)[i] = (unsigned char)(i % 256);
  }
}

int check(void * p, size_t size) {
  for (size_t i=0; i < size; i++) {
    if (((unsigned char *)p)[i] != 0) {
      return -1;
    }
  }
  return 0;
}

int main() {
  unsigned char buf[16384];
  for (size_t size=0; size <= 16384; size++) {
    // hs_memzero
    fill(buf, size);
    hs_memzero(buf, size);
    if (check(buf, size) != 0) {
        printf("hs_memzero fail\n");
        return 1;
    }

    // hs_memzero_finalizerEnv
    fill(buf, size);
    hs_memzero_finalizerEnv(&size, buf);
    if (check(buf, size) != 0) {
        printf("hs_memzero_finalizerEnv fail\n");
        return 2;
    }

    // hs_memzero_finalizerEnvFree
    size_t * psize = malloc(sizeof(size_t));
    if (psize == NULL) {
        printf("malloc fail\n");
        return 3;
    }
    *psize = size;
    fill(buf, size);
    hs_memzero_finalizerEnvFree(psize, buf);
    if (check(buf, size) != 0) {
        printf("hs_memzero_finalizerEnvFree fail\n");
        return 4;
    }
  }

  printf("ok\n");
}
