#include "cache_size.hpp"
#include "reg_availible.hpp"
#include "vector_reg_availible.hpp"
#include "bandwidth.hpp"
#include "create_file.hpp"
#include "eliminate.hpp"
#include "topology_parse.hpp"
extern "C" {
#include "stream2.h"
#include "stream.h"
}
#include <stdio.h>

int main () {
  int* cache_sizes;
  int* cache_sizes2;
  int regs;
  int vec_regs;
  int caches[20];
  int disputes[20];
  long long* bandwidths;
  double **benchmark_data;
  double **benchmark_data2;
  int num_caches = 0;
  int num_caches2 = 0;
  int points_sampled = 128;
  int sizes[128];
  fprintf(stderr, "finding availible registers...\n");
  regs = reg_avail(0);
  fprintf(stderr, "finding availible vector registers...\n");
  vec_regs = vector_reg_avail();
  fprintf(stderr, "profiling system...\n");
  benchmark_data = stream2(points_sampled, sizes);
  fprintf(stderr, "finding cache sizes...\n");
  cache_sizes = find_knees(&num_caches, benchmark_data, sizes, points_sampled);
//  for(int i = 0; i < num_caches; i++)
//    printf("\nsize: %d\n", cache_sizes[i]);
  fprintf(stderr, "cache sizes...\n");
  eliminate_small_changes(&num_caches, benchmark_data, sizes, points_sampled, cache_sizes);
  for(int i = 0; i < num_caches; i++)
    printf("size: %d\n", cache_sizes[i]);
  //cache_sizes = find_caches(&num_caches);
  fprintf(stderr, "running confirmation test...\n");
  fprintf(stderr, "profiling system...\n");
  benchmark_data2 = stream2(points_sampled, sizes);
  fprintf(stderr, "finding cache sizes...\n");
  cache_sizes2 = find_knees(&num_caches2, benchmark_data2, sizes, points_sampled);
//  for(int i = 0; i < num_caches2; i++)
  //  printf("\nsize: %d\n", cache_sizes2[i]);
  fprintf(stderr, "cache sizes...\n");
  eliminate_small_changes(&num_caches2, benchmark_data2, sizes, points_sampled, cache_sizes2);
  for(int i = 0; i < num_caches2; i++)
    printf("size: %d\n", cache_sizes2[i]);
  //cache_sizes[num_caches] = cache_sizes[num_caches-1];
  //cache_sizes[num_caches-1] = 1000000;
  //num_caches++;
  fprintf(stderr, "confirmed caches\n");
  int j = 0;
  int k = 0;
  int l = 0;
  for(int i = 0; i < num_caches; i++) { //compare the two profiles and keep track of differences
    if(cache_sizes[i] == cache_sizes2[j]) {
      printf("size: %d\n", cache_sizes2[j]);
      caches[k] = cache_sizes2[j];
      j++;
      k++;
      if(j == num_caches2) {
        i++;
        for(int m = i; m < num_caches; m++) {
          //printf("dispute2: %d\n", cache_sizes[m]);
          disputes[l] = cache_sizes2[m];
          l++;
        }
        break;
      }
    }
    else if(cache_sizes2[j] < cache_sizes[i]) {
      disputes[l] = cache_sizes2[j];
      l++;
      //printf("dispute3: %d\n", cache_sizes2[j]);
      j++;
      if(j == num_caches2) {
        for(int m = i; m < num_caches; m++) {
        //  printf("dispute4: %d\n", cache_sizes[m]);
          disputes[l] = cache_sizes2[j];
          l++;
        }
        break;
      }
      i--;
    }
    else {
      disputes[l] = cache_sizes[i];
      l++;
    //  printf("dispute5: %d\n", cache_sizes[i]);
    }
  }
  if(j != num_caches2) {
    for(int i = j; i < num_caches2; i++) {
      //printf("dispute: %d\n", cache_sizes2[i]);
      disputes[l] = cache_sizes2[i];
      l++;
    }
  }   
  if(l != 0) { //two profiles didn't match
    fprintf(stderr, "disputed sizes...\n");
    for(int i = 0; i < l; i++)
      fprintf(stderr, "%d\n", disputes[i]);
    num_caches = 0;
    fprintf(stderr, "resolving disputes...\n");
    fprintf(stderr, "profiling system...\n");
    benchmark_data = stream2(points_sampled, sizes);
    fprintf(stderr, "finding cache sizes...\n");
    cache_sizes = find_knees(&num_caches, benchmark_data, sizes, points_sampled);
    for(int i = 0; i < num_caches; i++)
      printf("\nsize: %d\n", cache_sizes[i]);
    fprintf(stderr, "cache sizes...\n");
    eliminate_small_changes(&num_caches, benchmark_data, sizes, points_sampled, cache_sizes);
    for(int i = 0; i < num_caches; i++)
      printf("size: %d\n", cache_sizes[i]);
    for(j = 0; j < l; j++) {
      for(int i = 0; i < num_caches; i++) {
        if(disputes[j] == cache_sizes[i]) {
          caches[k] = cache_sizes[i];
          k++; 
        }       
      }
    }
  }
  fprintf(stderr, "final cache sizes...\n");
  for(int i = 0; i < k; i++)
      printf("size: %d\n", caches[i]);
  fprintf(stderr, "determining bandwidths...\n");
  bandwidths = find_bandwidth(cache_sizes, num_caches);
  fprintf(stderr, "writing machine file...\n");
  create_file(caches, bandwidths, num_caches, regs, vec_regs);

  return 0;
}
