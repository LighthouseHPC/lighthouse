#include "machines.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

//This is Geoff's machine
struct machine* create_clovertown(){
  struct machine *ret;
  long long i;
  ret = malloc(sizeof(struct machine));
  ret->name = malloc(sizeof(char)*11);
  strcpy(ret->name, "Clovertown\0");
  ret->numcaches = 3;
  ret->caches = malloc(sizeof(struct caches*)*ret->numcaches);
  ret->caches[0] = create_cache(32*1024, 64, 8, "L1\0", 13481701376);	
  ret->caches[1] = create_cache(4*1024*1024, 64, 16, "L2\0", 3957388288);	
  ret->caches[2] = create_cache(1024*1024, 4096, 256, "TLB\0", 12798184576);	
  return ret;
};

//quadfather
struct machine* create_quadfather(){
  struct machine *ret;
  long long i;
  ret = malloc(sizeof(struct machine));
  ret->name = malloc(sizeof(char)*11);
  strcpy(ret->name, "Quadfather\0");
  ret->numcaches = 4;
  ret->caches = malloc(sizeof(struct caches*)*ret->numcaches);
  //ret->caches[0] = create_cache(32*1024, 64, 8, "L1\0", 5540848640);
  //ret->caches[0] = create_cache(32*1024, 64, 8, "L1\0", 8606000000);
  ret->caches[0] = create_cache(32*1024, 64, 8, "L1\0", 13481701376);	
  ret->caches[1] = create_cache(4*1024*1024, 64, 16, "L2\0", 2689525555);
  //ret->caches[2] = create_cache(1024*1024, 4096, 256, "TLB\0", 5486841651);
  //ret->caches[2] = create_cache(1024*1024, 4096, 256, "TLB\0", 8606000000);
  ret->caches[2] = create_cache(1024*1024, 4096, 256, "TLB\0", 12798184576);	
  ret->caches[3] = create_cache(6*8, 8, 256, "REG\0", 112798184576);	
  return ret;
};
/*
//opteron k8
struct machine* create_opteron(){
  struct machine *ret;
  long long i;
  ret = malloc(sizeof(struct machine));
  ret->name = malloc(sizeof(char)*8);
  strcpy(ret->name, "Opteron\0");
  ret->numcaches = 4;
  ret->caches = malloc(sizeof(struct caches*)*ret->numcaches);
  ret->caches[0] = create_cache(64*1024, 64, 2, "L1\0", 8987800576);
  ret->caches[1] = create_cache(1024*1024+64*1024, 64, 8, "L2\0", 3441715610);
  ret->caches[2] = create_cache(40*4096, 4096, 40, "TLB\0", 9769175936);
  ret->caches[3] = create_cache(2*1024*1024, 4096, 4, "TLB2\0", 3233434931);
  return ret;
};
*/
struct machine* create_opteron_low(){
  struct machine *ret;
  long long i;
  ret = malloc(sizeof(struct machine));
  ret->name = malloc(sizeof(char)*8);
  strcpy(ret->name, "Opteron\0");
  ret->numcaches = 4;
  ret->caches = malloc(sizeof(struct caches*)*ret->numcaches);
  ret->caches[0] = create_cache(64*1024, 64, 2, "L1\0", 3595567104);
  ret->caches[1] = create_cache(1024*1024+64*1024, 64, 8, "L2\0", 1811939328);
  ret->caches[2] = create_cache(40*4096, 4096, 40, "TLB\0", 3747610624);
  ret->caches[3] = create_cache(2*1024*1024, 4096, 4, "TLB2\0", 1784676352);
  return ret;
};

struct machine* create_opteron_mid(){
  struct machine *ret;
  long long i;
  ret = malloc(sizeof(struct machine));
  ret->name = malloc(sizeof(char)*8);
  strcpy(ret->name, "Opteron\0");
  ret->numcaches = 4;
  ret->caches = malloc(sizeof(struct caches*)*ret->numcaches);
  ret->caches[0] = create_cache(64*1024, 64, 2, "L1\0", 8987800576);
  ret->caches[1] = create_cache(1024*1024+64*1024, 64, 8, "L2\0", 1811939328);
  ret->caches[2] = create_cache(40*4096, 4096, 40, "TLB\0", 9769175936);
  ret->caches[3] = create_cache(2*1024*1024, 4096, 4, "TLB2\0", 1784676352);
  return ret;
};
/*
//nahalum i7
struct machine* create_i7(){
  struct machine *ret;
  long long i;
  ret = malloc(sizeof(struct machine));
  ret->name = malloc(sizeof(char)*3);
  strcpy(ret->name, "i7\0");
  ret->numcaches = 5;
  ret->caches = malloc(sizeof(struct caches*)*ret->numcaches);
  ret->caches[0] = create_cache(32*1024, 64, 8, "L1\0", 6089080832);
  ret->caches[1] = create_cache(256*1024, 64, 8, "L2\0", 5961154560);
  ret->caches[2] = create_cache(8*1024*1024, 64, 16, "L3\0", 5579472896);
  ret->caches[3] = create_cache(64*4096, 4096, 64, "TLB\0", 5992611840);
  ret->caches[4] = create_cache(2*1024*1024, 4096, 4, "TLB2\0", 5925502976);
  return ret;
};*/

//create a cache
struct cache* create_cache(long long size, long long linesize, long long associativity, char* name, long long bandwidth){
  struct cache *ret;
  ret = malloc(sizeof(struct cache));
  ret->name = malloc(sizeof(char)*(strlen(name)+1));
  strcpy(ret->name, name);
  ret->size = size;
  ret->linesize = linesize;
  ret->associativity = associativity;
  ret->lines = size/linesize;
  ret->bandwidth = bandwidth;
  return ret;
};

//Prints the number of misses to all levels of the memory heirachy
void print_misses(struct machine *in, long long *misses){
  long long i;
  for(i = 0; i < in->numcaches; i++)
	printf("%lld ", misses[i]);
  printf("\n");
  return;
};

void delete_machine(struct machine *in) {
  int i;
  for(i = 0; i < in->numcaches; i++)
    delete_cache(in->caches[i]);
  free(in->name);
  free(in->caches);
  free(in);
  return;
}

void delete_cache(struct cache *in) {
  free(in->name);
  free(in);
  return;
}
