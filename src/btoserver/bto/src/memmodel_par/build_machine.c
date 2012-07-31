#include "../../hardware_profiling/topology_parse.hpp"
extern "C" {
#include "parallel_machines.h"
}
#include <stdio.h>
#include <stdlib.h>
#include "build_machine.h"
#include <string.h>

struct machine* build_machine(char *pathToTop, int serial) {
	// pathToTop requires / at end.  as in path/
  struct machine* ret;
  struct machine* ptr;
  int num;
  int i, j;
  long long* bandwidths;
  FILE *my_file;
  int values[3];
  long long sizes[1];
  char **caches;
  caches = (char**)malloc(sizeof(char*));
  caches[0] = (char*)malloc(sizeof(char)*4);
	
//   fprintf(stderr, "cost %d\n", num);
  ret = topology_parse(pathToTop);
  // fprintf(stderr, "cost %d\n", num);
  compress_machine(ret);
  // fprintf(stderr, "cost %d\n", num);
  ptr = ret; 
  while(ptr->children != NULL) {
    ptr = ptr->children;
  }
	
  char *regsPath = (char*)malloc(sizeof(char)*(1+strlen(pathToTop)+
									strlen("hardware_profiling/regs.temp")));
  strcpy(regsPath,pathToTop);
  strcat(regsPath,"hardware_profiling/regs.temp");
  my_file=fopen(regsPath, "r");
  free(regsPath);
  if (my_file == NULL) {
        printf("Parallel analytic model failed to open regs.temp\n");
		printf("%s\n",pathToTop);
		printf("%s\n",regsPath);
        return NULL;
  }
  fscanf(my_file, "%d", &num);
  sizes[0] = num*8;
  strcpy(caches[0], "REG");
  ptr = add_child(ptr, 1, caches, sizes, ptr->cores, ptr->threads, ptr->sockets);
  fclose(my_file);
	
  char *bandPath = (char*)malloc(sizeof(char)*(1+strlen(pathToTop)+
									strlen("hardware_profiling/bandwidths.temp")));
  strcpy(bandPath,pathToTop);
  strcat(bandPath,"hardware_profiling/bandwidths.temp");
  my_file=fopen(bandPath, "r");
  free(bandPath);
  if (my_file == NULL) {
	printf("Parallel analytic model failed to open bandwidths.temp\n");
	return NULL;
  }
	
  fscanf(my_file, "%d", &num);
   //fprintf(stderr, "cost %d\n", num);
  bandwidths = (long long*)malloc(num*sizeof(long long));
  for(i = 0; i < num; i++)
    fscanf(my_file, "%lld", &bandwidths[i]);
  ptr = ret;
  j = num-1;
  //print_machine(ret);
  perculate_up(ret, values);
  while(ptr != NULL) {
   //fprintf(stderr, "cost %d\n", j);
    for(i = 0; i < ptr->numcaches; i++) {
      ptr->caches[i]->bandwidth = bandwidths[j];
      j--;
    }
    ptr = ptr->children;
  } 
  fclose(my_file);
  ptr = ret;
   //fprintf(stderr, "cost\n");
  //print_machine(ret);
  if(serial) {
    while(ptr != NULL) {
      ptr->threads = 1;
      ptr->compressed = 0;
      ptr->cores = 1;
      if(ptr->sockets > 1)
	ptr->sockets = 1;
      ptr = ptr->children;
    }
  } 
  return ret;
}
