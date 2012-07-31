#include "bandwidth.hpp"
#include <stdio.h>
#include <iostream>
#include "../src/memmodel_par/parallel_machines.h"
#include <stdlib.h>
extern "C" {
#include "stream.h"
}

//figures out the bandwidth of identified memory structures
long long* find_bandwidth(int* cache_sizes, int caches) {
  int ntimes = 10;
  long long* bandwidths = new long long [caches+1];
  //std::cout << caches << "\n";
  bandwidths[0] = stream(cache_sizes[0]/(3*sizeof(double)), 1000);
  //     std::cout << " " << cache_sizes[0] << " " <<  bandwidths[0] << "\n"; 
  for(int i = 0; i < caches - 1; i++){
	bandwidths[i+1] = stream(((cache_sizes[i] + cache_sizes[i+1])/2)/(3*sizeof(double)), ntimes);
    //   std::cout << cache_sizes[i] << " " << cache_sizes[i+1] << " " << bandwidths[i+1] << "\n"; 
  }
  bandwidths[caches] = stream(cache_sizes[caches-1]*3, ntimes);
      // std::cout << cache_sizes[caches-2] << " " << " "  << bandwidths[caches-1] << "\n"; 
  return bandwidths;
}

long long* par_bandwidths(struct machine *in, int *caches) {
  (*caches) = 0;
  struct machine *ptr;
  int cache_sizes[10];
  ptr = in;
  for(int i = 0; i < ptr->numcaches; i++) {
    cache_sizes[*caches] = ptr->caches[i]->size;
    (*caches)++;
  }
  while(ptr->children != NULL) {
    ptr = ptr->children;
    for(int i = 0; i < ptr->numcaches; i++) {
      cache_sizes[*caches] = ptr->caches[i]->size;
      (*caches)++;
    }
  }
  sort_caches(cache_sizes, *caches);
  long long *ret = find_bandwidth(cache_sizes, *caches);
  (*caches)++;
  return ret;
}

void sort_caches(int* cache_sizes, int caches) {
  int temp;
  for(int i = 0; i < caches; i++)
    for(int j = 0; j < caches-1; j++)
      if(cache_sizes[j] > cache_sizes[j+1]) {
	temp = cache_sizes[j+1];
	cache_sizes[j+1] = cache_sizes[j];
	cache_sizes[j] = temp;
      }
  return;
}
