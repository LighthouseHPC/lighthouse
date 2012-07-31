#ifndef COST_H
#define COST_H

#include "machines.h"
#include "tree.h"
#include "memmodel_clean.h"

enum COST_CALC_TYPES {
  PASSED_VALS,
  MACHINE_TYPE, 
  MEM_STRUCT_TYPE,
  TRY_IN_ORDER,
  DEFAULT
} calc_types;

double new_cost(struct machine *, struct node *); 
long long* sumvars(struct machine *, struct node *); 
double min_loop_cost(struct machine *, long long*);
double cost(long long *, struct machine *, int, double *);
double calc_passed(long long *, double *, long long);
double calc_mem_struct(long long *, struct machine *);
double calc_machine(long long *, struct machine *);
double mem_struct_cost_lookup(struct cache *, char *);
double* machine_cost_lookup(char*);

#endif
