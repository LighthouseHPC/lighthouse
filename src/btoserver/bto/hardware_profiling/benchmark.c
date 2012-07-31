#include "benchmark.h"

void Triad(double *a, double *b, double *c, double scalar, int N) {
  int j;
  for(j = 0; j < N; j++)
    a[j] = b[j]+scalar*c[j];
  return;
}
