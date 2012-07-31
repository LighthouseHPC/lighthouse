#include "your_machine.h"
#include "machines.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

struct machine* your_machine(){
  struct machine * ret;
  ret = malloc(sizeof(struct machine));
  ret->name = malloc(sizeof(char)*12);
  strcpy(ret->name, "YourMachine\0");
  ret->numcaches = 3;
  ret->caches = malloc(sizeof(struct caches*)*3);
  ret->caches[0] = create_cache(4194304, 64, 8, "L1\0", 4261474407);
  ret->caches[1] = create_cache(32768, 64, 8, "L2\0", 13997101143);
  ret->caches[2] = create_cache(48, 64, 8, "Reg\0", 17415908975);
  return ret;
}