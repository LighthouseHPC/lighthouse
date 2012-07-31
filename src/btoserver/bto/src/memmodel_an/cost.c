#include "cost.h"
#include <stdlib.h>
#include <stdio.h>

double new_cost(struct machine *inmachine, struct node* inloop) {
  double cost = 0.0;
  double cost2 = 0.0;
  long long* misses;
  int i;
  if(inloop->its == 0)
	return 0;
  misses = sumvars(inmachine, inloop);
  //print_misses(inmachine, misses);
  //misses = all_misses(inmachine, inloop);
  cost = min_loop_cost(inmachine, misses);
  free(misses);
  //printf("cost in %lf\n", cost);
  if(cost > 0) {
    for(i = 0; i < inloop->numchildren; i++){
	  cost2 += new_cost(inmachine, inloop->children[i]);
       //printf("cost2 in %lf %d\n", cost, i);
	}
    if(cost2 > cost){
        //printf("RET cost2 in %lf\n", cost2);

	  return cost2;
	}
  }
  //printf("RET cost in %lf\n", cost);
  return cost;
}

long long* sumvars(struct machine *inmachine, struct node* inloop) {
  long long* misses;
  int i, j;
  //printf("here\n");
  misses = malloc(inmachine->numcaches*sizeof(long long));
  for(i = 0; i < inmachine->numcaches; i++) {
    //printf("here2\n");
	misses[i] = 0;
	for(j = 0; j < inloop->variables; j++) {
	  misses[i] += inloop->vars[j]->misses[i];
//          printf("var %lld\n", inloop->vars[j]->misses[i]);
	}
  }
  return misses;
}

double min_loop_cost(struct machine *inmachine, long long* misses) {
  double cost = 0;
  int i;
  for(i = 0; i < inmachine->numcaches; i++) {
    //printf("cost in2 %lf\n", cost);
//	printf("eval %lf\n", (double)(misses[i]*inmachine->caches[i]->linesize)/(double)inmachine->caches[i]->bandwidth);
	if((double)(misses[i]*inmachine->caches[i]->linesize)/(double)inmachine->caches[i]->bandwidth > cost)
	  cost = (double)(misses[i]*inmachine->caches[i]->linesize)/(double)inmachine->caches[i]->bandwidth;
  }
  return cost;
}

double cost(long long *misses, struct machine *inmachine, int calctype, double *costs) {
  double totalcost = 0.0;
  switch(calctype) {
	case PASSED_VALS : totalcost = calc_passed(misses, costs, inmachine->numcaches);
  					   break;
	case MACHINE_TYPE : totalcost = calc_machine(misses, inmachine);
  						break;
	case MEM_STRUCT_TYPE : totalcost = calc_mem_struct(misses, inmachine);
  						   break;
	case TRY_IN_ORDER : if(costs != NULL) 
  						  totalcost = calc_passed(misses, costs, inmachine->numcaches);
						if(totalcost < 0)
						  calc_machine(misses, inmachine);
						if(totalcost < 0) 
						  calc_mem_struct(misses, inmachine);
						break;
	default : totalcost = calc_mem_struct(misses, inmachine);
  }
  return totalcost;
}

double calc_passed(long long *misses, double *costs, long long size) {
  double totalcost = 0.0;
  int i;
  if(costs == NULL)
	return -1;
  for(i = 0; i < size; i++)
	totalcost += (double)misses[i]*costs[i];
  return totalcost;
}

double calc_mem_struct(long long *misses, struct machine *inmachine) {
  double totalcost = 0.0;
  int i;
  for(i = 0; i < inmachine->numcaches; i++)
	totalcost += (double)misses[i] * mem_struct_cost_lookup(inmachine->caches[i], inmachine->caches[i]->name);
  return totalcost;
}

double calc_machine(long long *misses, struct machine *inmachine) {
  double totalcost = 0.0;
  double *costs;
  costs = machine_cost_lookup(inmachine->name);
  totalcost = calc_passed(misses, costs, inmachine->numcaches);
  return totalcost;
}

double mem_struct_cost_lookup(struct cache *incache, char *name) {
  if(strcmp(name, "L2") == 0)
	return 1.0;
  return 0.0;
}

double* machine_cost_lookup(char *machine_name){
  double *costs;
   //data lookup routines here for now NULL and not used
  costs = NULL;
  return costs;
}
