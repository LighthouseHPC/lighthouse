#include "eliminate.hpp"
#include <stdio.h>

//removes and caches that do not have at least effect bandwidth by 5%
void eliminate_small_changes(int *num_caches, double **benchmark_data, int *sizes, int points_sampled, int *caches_sizes) {
  int i, j, k;
  double cur_bandwidth;
  double new_bandwidth;
  int mid;
  i = 0;
  while(i < points_sampled && caches_sizes[i] < caches_sizes[0]/2)
    i++;
  cur_bandwidth = sum_bench(benchmark_data, i);
  for(j = 0; j < (*num_caches); j++) {
    if(j + 1 < (*num_caches))
      mid = (caches_sizes[j] + caches_sizes[j+1]) / 2;
    else
      mid = num_caches[j] * 2;
    while(sizes[i] < mid && i < points_sampled)
      i++;
    new_bandwidth = sum_bench(benchmark_data, i);
    if(cur_bandwidth*.95 < new_bandwidth) {
      for(k = j + 1; k < (*num_caches); k++)
	caches_sizes[k-1] = caches_sizes[k];
      (*num_caches)--; 
      cur_bandwidth = new_bandwidth;
    }
  }
  return;
}

//sum up all bandwidths from all benchmarks
double sum_bench(double **benchmark_data, int place) {
  int i;
  double sum = 0.0;
  for(i = 0; i < 4; i++)
    sum += benchmark_data[i][place];
  return sum;
}
