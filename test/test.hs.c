#include "test.hs.h"

void test_finalizerPtrEnvSet1(int * env, void * p) {
  *env = 1;
}
