#ifndef MACHINES_H
#define MACHINES_H

struct machine{
  char *name;
  long long numcaches;
  struct cache **caches;
  long long cycles; //clock speed in hz
  int flops; //number of floating point operations capable of being performed per cycle
};

struct cache{
  char* name;
  long long size; //in bytes
  long long linesize; //in bytes
  long long associativity;
  long long lines;
  long long bandwidth; //in bytes per second from next larger structure to this one
  long long latency; //in clock cycles for some quantities this may need to be approximate this is not used yet
};

struct cache* create_cache(long long, long long, long long, char*, long long);
void print_misses(struct machine*, long long*);
/*struct machine* create_clovertown();
struct machine* create_quadfather();
struct machine* create_i7();
struct machine* create_opteron();
struct machine* create_opteron_low();
struct machine* create_opteron_mid();
struct machine* create_northwood();*/
void delete_machine(struct machine*);
void delete_cache(struct cache*);

#endif
