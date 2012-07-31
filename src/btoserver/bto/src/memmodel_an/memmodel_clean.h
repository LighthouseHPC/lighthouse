#ifndef MEMMODEL_H
#define MEMMODEL_H

#include "machines.h"
#include "tree.h"

#define wordsize 8

long long* all_misses(struct machine*, struct node*); //clean
long long mem_misses_structure(struct node*, long long, long long, int, struct node*); //clean
long long calc_working_set(struct node*); //clean
long long calc_reuse_distance(struct node*, int);  //clean
struct var** find_distinct_vars(struct node*, int *); //clean
long long calc_var_size(struct node*, struct var*); //clean
int var_used_once(struct node*, int); //clean
int var_iterated_over(struct node*, int);  //clean
int var_always_iterated_over(struct node*, int); //clean
int first_access(struct node*, int); //clean
int find_last_access(struct node*, int); //clean
int find_previous_access(struct node*, int); //clean
int find_next_access(struct node*, int); //clean
int child(struct node*, int); //clean
int child_location(struct node*, int); //clean
long long after_var(struct node*, int, char**, long long*, int); //buggy
long long before_var(struct node*, int); //clean
long long calc_working_set_plus_its(struct node*, char**, long long*, int); //clean
long long calc_var_offits(struct var*, char**, long long*, int); //clean
int in_different_subloop(struct node*, int, int); //clean
long long reuse_other_loops(struct node*, int); //clean
int used_in_same(struct node*, int); //works for what I tested it on but may not work in all cases I'm skeptical of this function
void find_smallest(long long, struct node*, int, long long*);
int iterated_above(struct node*, long long, char **);

#endif
