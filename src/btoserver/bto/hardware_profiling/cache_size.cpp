#include "cache_size.hpp"
#include <sys/time.h>
#include <stdio.h>
#include <stdlib.h>

double mysecond();

//Figures out where performance drops off and identifies the steepest point 
//in that dropoff.  Calls the cache size the closest realistic size to that
//steepest point.
int* find_knees(int *num_caches, double **benchmark_data, int *sizes, int points) {
  int check[4][points-4];
  int i, j;
  double a, ratio;
  int spot;
  int* cache_sizes;
  int count;
  int next;
  cache_sizes = new int[50];
  for(i = 0; i < 4; i++) //finds points where a point is at least 1% less than the moving average of the previous 4 points and itself.
    for(j = 0; j < points - 4; j++)
      if(benchmark_data[i][j+4]*1.01 < ((benchmark_data[i][j] + benchmark_data[i][j+1] + benchmark_data[i][j+2] + benchmark_data[i][j+3] + benchmark_data[i][j+4]))/5)
	check[i][j] = 1;
      else
	check[i][j] = 0;
  for(i = 0; i < 4; i++) //finds when three in a row are below the moving average for a benchmark
    for(j = 0; j < points - 6; j++)
      if(check[i][j] && check[i][j+1] && check[i][j+2]) {
	check[i][j+1] = 2;
      }
  for(j = 1; j < points - 5; j++) { //If 2 of 3 of the first benchmark and the DAXPY have 3 in a row we have a cache
    count = 0;
    for(i = 0; i < 3; i++)
      if(check[i][j] == 2)
	count++;
    if(count >= 2) {
      if(check[3][j] == 2) {
        check[0][j] = 3;
      }
    }
  }
  for(j = 1; j < points - 5; j++) { //find biggest droppoff and set to nearest cache
    if(check[0][j] == 3) {
      ratio = 4;
      spot = j;
      while(check[0][j] == 3) { //find droppoff
        a = ((benchmark_data[0][j+4]/benchmark_data[0][j+3]) + (benchmark_data[1][j+4]/benchmark_data[1][j+3]) + (benchmark_data[2][j+4]/benchmark_data[2][j+3]) + (benchmark_data[3][j+4]/benchmark_data[3][j+3]));
        if(a < ratio) {
          ratio = a;
          spot = j;
	}
        j++;
      }
      cache_sizes[(*num_caches)] = nearest_cache(sizes[spot+4]*16); //set cache
      if(*num_caches) {  //if new cache save it
        if(cache_sizes[(*num_caches)] != cache_sizes[(*num_caches)-1])
          (*num_caches)++;
      }
      else
        (*num_caches)++;
    }
  }
  return cache_sizes;
}

int nearest_cache(int in_size) {
  int size = 1024;
  int diff, last;
  diff = 1000000000;
  while(size *= 2) {
    last = diff;
    diff = abs(in_size - size);
    if(last < diff)
      return ((size/4)*3);
    last = diff;
    diff = abs(in_size - (size/2)*3);
    if(last < diff)
      return size; 
  }
  return size;
}

//should pass in cache_max and allow redefinition
//old function
int* find_caches(int *num_caches) {
  long long i = 1024;
  //ong long cache_max = 268435456;
  //long long cache_max = 268435456;
  long long cache_max = 16777216;
  double prev;
  double *test_array;
  int arraysize;
  double time;
  int x = 1;
  double cur, last, next;
  int line_size;
  int* caches;
  *num_caches = 0;
  caches = new int[100];
  test_array = new double [cache_max];
  //line size
  for(int k = 0; k < cache_max; k++)
	test_array[k] = k;
  for(i = 1; i <= 4096; i*=2) {
	time = mysecond();
	for(int j = 0; j < 100; j++)
	  for(int k = 0; k < cache_max; k+=i)
	    test_array[k] *= test_array[k];
   	time = mysecond() - time;
	time *= i;
	//fprintf(stderr, "%lld %lf\n", i, time);
	if(i == 1)
	  cur = time;
	else {
	  last = cur;
	  cur = time;
	  if(last*1.5 >= cur) {
		line_size = i/2;
	    break;
	  }
	}
  }
  //find_lines
  int found;
  for(int k = 64; k < cache_max/line_size; k *= 2) {
	time = mysecond();
	found = 0;
	int factor = k/64;
	for(int l = 0; l < (125000*line_size)/factor; l++)
	  for(int j = line_size; j < k; j+=line_size)
	    test_array[j+3] *= test_array[j];	
   	time = mysecond() - time;
	time = (time/k)*100000*(factor+1);
	//fprintf(stderr, "%d %lf\n", k*line_size, time);
    if(k == 64) {
	  next = time;
	}
	else {
	  last = cur;
	  cur = next;
	  next = time;
	  if((cur/1.2 > last) && (next*1.05 > cur)) {
		if(caches[(*num_caches-1)]*2 > (k/2)*line_size)
		  (*num_caches)--;
		caches[(*num_caches)] = (k/2)*line_size;
		(*num_caches)++;
		found = 1;
	  }
	}
	time = mysecond();
	for(int l = 0; l < (125000*2*line_size)/(factor*3); l++)
	  for(int j = line_size; j < (k*1.5); j+=line_size)
	    test_array[j] *= test_array[j];	
   	time = mysecond() - time;
	time = (time/k)*100000*(factor);
	//fprintf(stderr, "%d %lf\n", (k*3*line_size)/2, time);
	if(k == 64) {
	  cur = next;
	  next = time;
	}
	else {
	  last = cur;
	  cur = next;
	  next = time;
	  if((cur/1.2 > last) && (next*1.05 > cur) && (found == 0)) {
		caches[(*num_caches)] = ((k*3)/4)*line_size;
		(*num_caches)++;
	  }
	}
  }
  //for(i = 0; i < (*num_caches); i++)
	//fprintf(stderr, "%d\n", caches[i]);
  return caches;
}

double mysecond()
{
        struct timeval tp;
        struct timezone tzp;
        int i;

        i = gettimeofday(&tp,&tzp);
        return ( (double) tp.tv_sec + (double) tp.tv_usec * 1.e-6 );
}

