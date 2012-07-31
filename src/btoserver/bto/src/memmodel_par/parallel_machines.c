#include "parallel_machines.h"
#include "tree_par.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

//This is Geoff's machine
/*struct machine* create_clovertown(){
  struct machine *ret;
  long long i;
  ret = malloc(sizeof(struct machine));
  ret->name = malloc(sizeof(char)*11);
  strcpy(ret->name, "Clovertown\0");
  ret->numcaches = 3;
  ret->caches = malloc(sizeof(struct caches*)*ret->numcaches);
  ret->caches[0] = create_cacheP(32*1024, 64, 8, "L1\0", 13481701376);	
  ret->caches[1] = create_cacheP(4*1024*1024, 64, 16, "L2\0", 3957388288);	
  ret->caches[2] = create_cacheP(1024*1024, 4096, 256, "TLB\0", 12798184576);	
  return ret;
};*/

//quadfather
struct machine* create_quadfatherP(){
  struct machine *ret;
  long long i;
  ret = malloc(sizeof(struct machine));
  ret->name = malloc(sizeof(char)*11);
  strcpy(ret->name, "Quadfather\0");
  ret->numcaches = 4;
  ret->caches = malloc(sizeof(struct caches*)*ret->numcaches);
  //ret->caches[0] = create_cacheP(32*1024, 64, 8, "L1\0", 5540848640);
  //ret->caches[0] = create_cacheP(32*1024, 64, 8, "L1\0", 8606000000);
  ret->caches[0] = create_cacheP(32*1024, 64, 8, "L1\0", 13481701376);	
  ret->caches[1] = create_cacheP(4*1024*1024, 64, 16, "L2\0", 2689525555);
  //ret->caches[2] = create_cacheP(1024*1024, 4096, 256, "TLB\0", 5486841651);
  //ret->caches[2] = create_cacheP(1024*1024, 4096, 256, "TLB\0", 8606000000);
  ret->caches[2] = create_cacheP(1024*1024, 4096, 256, "TLB\0", 12798184576);	
  ret->caches[3] = create_cacheP(6*8, 8, 256, "REG\0", 112798184576);	
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
  ret->caches[0] = create_cacheP(64*1024, 64, 2, "L1\0", 8987800576);
  ret->caches[1] = create_cacheP(1024*1024+64*1024, 64, 8, "L2\0", 3441715610);
  ret->caches[2] = create_cacheP(40*4096, 4096, 40, "TLB\0", 9769175936);
  ret->caches[3] = create_cacheP(2*1024*1024, 4096, 4, "TLB2\0", 3233434931);
  return ret;
};
*/
struct machine* create_opteron_lowP(){
  struct machine *ret;
  long long i;
  ret = malloc(sizeof(struct machine));
  ret->name = malloc(sizeof(char)*8);
  strcpy(ret->name, "Opteron\0");
  ret->numcaches = 4;
  ret->caches = malloc(sizeof(struct caches*)*ret->numcaches);
  ret->caches[0] = create_cacheP(64*1024, 64, 2, "L1\0", 3595567104);
  ret->caches[1] = create_cacheP(1024*1024+64*1024, 64, 8, "L2\0", 1811939328);
  ret->caches[2] = create_cacheP(40*4096, 4096, 40, "TLB\0", 3747610624);
  ret->caches[3] = create_cacheP(2*1024*1024, 4096, 4, "TLB2\0", 1784676352);
  return ret;
};

struct machine* create_opteron_midP(){
  struct machine *ret;
  long long i;
  ret = malloc(sizeof(struct machine));
  ret->name = malloc(sizeof(char)*8);
  strcpy(ret->name, "Opteron\0");
  ret->numcaches = 4;
  ret->caches = malloc(sizeof(struct caches*)*ret->numcaches);
  ret->caches[0] = create_cacheP(64*1024, 64, 2, "L1\0", 8987800576);
  ret->caches[1] = create_cacheP(1024*1024+64*1024, 64, 8, "L2\0", 1811939328);
  ret->caches[2] = create_cacheP(40*4096, 4096, 40, "TLB\0", 9769175936);
  ret->caches[3] = create_cacheP(2*1024*1024, 4096, 4, "TLB2\0", 1784676352);
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
  ret->caches[0] = create_cacheP(32*1024, 64, 8, "L1\0", 6089080832);
  ret->caches[1] = create_cacheP(256*1024, 64, 8, "L2\0", 5961154560);
  ret->caches[2] = create_cacheP(8*1024*1024, 64, 16, "L3\0", 5579472896);
  ret->caches[3] = create_cacheP(64*4096, 4096, 64, "TLB\0", 5992611840);
  ret->caches[4] = create_cacheP(2*1024*1024, 4096, 4, "TLB2\0", 5925502976);
  return ret;
};*/

//create a cache

struct machine* new_machine(int num_caches, char** caches, long long* sizes, int cores, int threads, int sockets, char* name) {
  struct machine *new_machine;
  int i;
  new_machine = malloc(sizeof(struct machine));  
  new_machine->name = malloc(sizeof(char)*(strlen(name)+1));
  strcpy(new_machine->name, name);
  new_machine->numcaches = num_caches;
  new_machine->compressed = 0;
  new_machine->cycles = 0;
  new_machine->flops = 0;
  new_machine->children = NULL;
  new_machine->sibling = NULL;
  new_machine->cores = cores;
  new_machine->threads = threads;
  new_machine->sockets = sockets;
  if(num_caches == 0)
    new_machine->caches = NULL;
  else {
    new_machine->caches = malloc(num_caches*sizeof(struct cache*));
    for(i = 0; i < num_caches; i++) 
      new_machine->caches[i] = create_cacheP(sizes[i], 1, 1, caches[i], 0);
  }
  return new_machine;
}

struct machine* add_child(struct machine* cur, int num_caches, char** caches, long long* sizes, int cores, int threads, int sockets) {
  struct machine *new_machine;
  int i;
  new_machine = malloc(sizeof(struct machine));
  new_machine->name = NULL;
  new_machine->numcaches = num_caches;
  new_machine->compressed = 0;
  new_machine->cycles = 0;
  new_machine->flops = 0;
  new_machine->children = NULL;
  new_machine->sibling = NULL;
  new_machine->cores = cores;
  new_machine->threads = threads;
  new_machine->sockets = sockets;
  if(num_caches == 0)
    new_machine->caches = NULL;
  else {
    new_machine->caches = malloc(num_caches*sizeof(struct cache*));
    for(i = 0; i < num_caches; i++)
      new_machine->caches[i] = create_cacheP(sizes[i], 1, 1, caches[i], 0);
  }
  cur->children = new_machine;
  return new_machine;
}

struct machine* add_sibling(struct machine* cur, int num_caches, char** caches, long long* sizes, int cores, int threads, int sockets) {
  struct machine *new_machine;
  int i;
  new_machine = malloc(sizeof(struct machine));
  new_machine->name = NULL;
  new_machine->numcaches = num_caches;
  new_machine->compressed = 0;
  new_machine->cycles = 0;
  new_machine->flops = 0;
  new_machine->children = NULL;
  new_machine->sibling = NULL;
  new_machine->cores = cores;
  new_machine->threads = threads;
  new_machine->sockets = sockets;
  if(num_caches == 0)
    new_machine->caches = NULL;
  else {
    new_machine->caches = malloc(num_caches*sizeof(struct cache*));
    for(i = 0; i < num_caches; i++)
      new_machine->caches[i] = create_cacheP(sizes[i], 1, 1, caches[i], 0);
  }
  cur->sibling = new_machine;
  return new_machine;
}

struct cache* create_cacheP(long long size, long long linesize, long long associativity, char* name, long long bandwidth){
  struct cache *ret;
  ret = malloc(sizeof(struct cache));
  ret->name = malloc(sizeof(char)*(strlen(name)+1));
  strcpy(ret->name, name);
  ret->size = size;
  ret->linesize = linesize;
  ret->associativity = associativity;
  if(linesize != 0)
    ret->lines = size/linesize;
  else
    ret->lines = 0;
  ret->bandwidth = bandwidth;
  ret->latency = 0;
  return ret;
};

//only compresses higher level if below is compressed as well
void compress_machine(struct machine *in) {
  int i;
  if(in->children != NULL){
    compress_machine(in->children);
  }
  if(in->sibling != NULL) {
    compress_machine(in->sibling);
    //printf("in\n");
    //print_machine(in);
    //printf("sibling\n");
  //print_machine(in->sibling);
	
    if(in->numcaches == in->sibling->numcaches && in->cores == in->sibling->cores && in->threads == in->sibling->threads && in->sockets == in->sibling->sockets) {
      for(i = 0; i < in->numcaches; i++)
        if(!cache_equal(in->caches[i], in->sibling->caches[i]))
	  return;
      if(in->children == NULL) {
	if(in->sibling->children != NULL)
	  return;
      }
      else if(in->sibling->children == NULL)
	return;
      else if(!children_equal(in->children, in->sibling->children))
	return;
      if(in->compressed == 0)
	in->compressed++;
      if(in->sibling->compressed == 0)
	in->compressed++;
      else
	in->compressed += in->sibling->compressed;
      delete_machineP(in->sibling);
      in->sibling = NULL;
    }
    else
      return;
  }
  return;
}

void perculate_up(struct machine* in, int *values) {
  int cur_values[3];
  cur_values[0] = 0;
  cur_values[1] = 0;
  cur_values[2] = 0;
  if(in->children != NULL) {
    perculate_up(in->children, cur_values);
    if(cur_values[0] != 0)
      in->cores = cur_values[0];
    else
      in->cores += cur_values[0];
    if(cur_values[1] != 0)
      in->threads = cur_values[1];
    else
      in->threads += cur_values[1];
    if(cur_values[2] != 0)
      in->sockets = cur_values[2];
    else
      in->sockets += cur_values[2];
  }
  if(in->sibling != NULL)
    perculate_up(in->sibling, cur_values);
  if(in->compressed) {
    values[0] += in->cores*in->compressed;
    values[1] += in->threads*in->compressed;
    values[2] += in->sockets*in->compressed;
  }
  else {
    values[0] += in->cores;
    values[1] += in->threads;
    values[2] += in->sockets;
  }
  return;
}

int cache_equal(struct cache *first, struct cache *second) {
  if(first->size != second->size || first->linesize != second->linesize || first->associativity != second->associativity || first->lines != second->lines || first->bandwidth != second->bandwidth || first->latency != second->latency)
    return 0;
  if(first->name == NULL) {
    if(second->name != NULL)
      return 0;
  }
  else if(second->name == NULL)
    return 0;
  if(strcmp(first->name, second->name))
    return 0;
  return 1;
}

int count_caches(struct machine *in) {
  int caches = 0;
  struct machine *ptr;
  ptr = in;
  if(ptr == NULL)
    printf("crap");  
while(ptr != NULL) {
     // fprintf(stderr, "ptr %d\n", ptr->numcaches);
    caches += ptr->numcaches;
    ptr = ptr->children;
  }
  return caches;
}

//counts the number of buses that can deliver data from a cache level to all processors
int buses_to_me(struct machine *me, struct machine *in, int cacheloc) {
  struct machine *ptr;
  int buses = 1;
  ptr = in;
      //fprintf(stderr, "buses %d\n", buses);
  while(ptr != me && ptr != NULL) {
      //fprintf(stderr, "here %d\n", buses);
    if(ptr->compressed)
      buses *= ptr->compressed;
    ptr = ptr->children;
  }
      //fprintf(stderr, "buses %d\n", buses);
  if(cacheloc > 0 && ptr->compressed != 0) {
    buses *= ptr->compressed;
  }
      //fprintf(stderr, "buses %d\n", buses);
  return buses;
} 

int children_equal(struct machine *first, struct machine *second) {
  if(first->numcaches != second->numcaches || first->compressed != second->compressed || first->cycles != second->cycles || first->flops != second->flops || first->cores != second->cores || first->threads != second->threads || first->sockets != second->sockets) 
    return 0;
  if(first->name == NULL) {
    if(second->name != NULL)
      return 0;
  }
  else if(second->name == NULL)
    return 0;
  else if(strcmp(first->name, second->name))
    return 0;
  return 1;
}

struct machine* par_to_serial(struct machine *in) {
  struct machine *serial;
  struct machine *ptr;
  int i;
  int j = 0;
  serial = malloc(sizeof(struct machine));
  serial->numcaches = count_caches(in);
  serial->caches = malloc(serial->numcaches*sizeof(struct cache*));
  ptr = in;
  while(ptr != NULL) {
   for(i = 0; i < ptr->numcaches; i++) {
     serial->caches[j] = create_cacheP(ptr->caches[i]->size, ptr->caches[i]->linesize, ptr->caches[i]->associativity, ptr->caches[i]->name, ptr->caches[i]->bandwidth);
     j++;
   }
   ptr = ptr->children;
  }
  serial->compressed = 0;
  serial->sockets = 1;
  serial->cores = 1;
  serial->threads = 1;
  serial->flops = in->flops;
  serial->cycles = in->cycles;
  strcpy(serial->name, in->name); 
  return serial;
}

void print_machine(struct machine* in) {
  int i;
  if(in->name != NULL)
    printf("Name: %s ", in->name);
  else
    printf("Node: %s ", in->name);
  printf("Cores: %d Threads: %d Sockets: %d Caches: %lld Compressed %d\n", in->cores, in->threads, in->sockets, in->numcaches, in->compressed);
  for(i = 0; i < in->numcaches; i++)
    print_cache(in->caches[i]);
  if(in->children != NULL) {
    printf("Child next\n");
    print_machine(in->children);
  }
  if(in->sibling != NULL) {
    printf("Sibling next\n");
    print_machine(in->sibling);
  }
    printf("up\n");
  return;
}

void print_cache(struct cache *in) {
  printf("Cache Name: %s Size: %lld bandwidth %lld\n", in->name, in->size, in->bandwidth);
  return;
}

//Prints the number of misses to all levels of the memory heirachy
void print_missesP(struct machine *in, struct node *tree){
  long long j, i, k;
  long long total = 0;
  struct machine* pointer;
  pointer = in;
  k = 0;
  while(pointer != NULL) {
    for(i = 0; i < pointer->numcaches; i++) {
      total = 0;
      for(j = 0; j < tree->variables; j++)
        total += tree->vars[j]->misses[k];
      printf("%lld ", total);
      printf(",");
      k++;
    }
    pointer = pointer->children;
  }
  return;
};

void delete_machineP(struct machine *in) {
  int i;
  if(in->children != NULL)
    delete_machineP(in->children);
  if(in->sibling != NULL)
    delete_machineP(in->sibling);
  for(i = 0; i < in->numcaches; i++)
    delete_cacheP(in->caches[i]);
  free(in->name);
  free(in->caches);
  free(in);
  return;
}

void delete_cacheP(struct cache *in) {
  free(in->name);
  free(in);
  return;
}
