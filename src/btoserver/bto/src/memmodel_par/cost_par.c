#include "cost_par.h"
#include "parallel_machines.h"
#include <stdlib.h>
#include <stdio.h>

//Stores the low and high estimates in an array in that order.
double* parallel_costs(struct machine *inmachine, struct node* inloop) {
  double *costs;
  int totalcaches = 0;
  totalcaches = count_caches(inmachine);
  //fprintf(stderr, "caches %d\n", totalcaches);
  //print_machine(inmachine);
  costs = malloc(2*sizeof(double));
  costs[1] = cost_high(inmachine, inloop, totalcaches, 0);
  //fprintf(stderr, "here %lf\n", costs[1]);
  if(inmachine->threads != 1)
    costs[0] = cost_low(inmachine, inloop, totalcaches, costs[1]);
  else
    costs[0] = 0.00001;
  //fprintf(stderr, "here %lf\n", costs[0]);
  return costs;
}

//A low end cost estimate assumes when calculations are bound on multiple things that
//the calculation is only bound on one structure since data traffic will be overlapped
//when parallel computation can occur.  When there is only a single path data is bound on
//different calculations the cost_high algorithm is used.
double cost_low(struct machine *inmachine, struct node* inloop, int totalcaches, double cost_hi) {
  long long* misses;
  double cost;
  int bound_spot;
  struct machine *bound_structure;
  struct machine *new_top;
  int new_caches;
  int i;
  misses = sumvarsP(inmachine, inloop, totalcaches, 0);
  cost = min_loop_costP(inmachine, misses, totalcaches, &bound_spot, &bound_structure);
  //     print_machine(inmachine);
    //   print_machine(bound_structure); 
   //   fprintf(stderr, "min_cost %lf\n", cost);
       //need function for procs below me
      //fprintf(stderr, "threads %d\n", bound_structure->cores);
     // fprintf(stderr, "busesxx %d\n", buses_to_me(bound_structure, inmachine, bound_spot));
  if(cost * (bound_structure->threads/buses_to_me(bound_structure, inmachine, bound_spot)) < cost_hi) {
  //then overlapping does not occur and must be accounted only for structures closer to the processor.  Other structures do not affect performance in this assumption scheme.
      //fprintf(stderr, "here\n");
     new_caches = count_caches(bound_structure) - bound_spot;
      //fprintf(stderr, "caches  spot %d %d\n", count_caches(bound_structure), bound_spot);
     if(bound_spot == 0) {
      //fprintf(stderr, "total - new %d\n", totalcaches - new_caches);
       cost = cost_high(bound_structure, inloop, totalcaches, totalcaches - new_caches);
     }
     else {
      //fprintf(stderr, "here3\n");
       new_top = malloc(sizeof(struct machine));
       new_top->numcaches = bound_structure->numcaches - bound_spot;
       new_top->caches = malloc(new_top->numcaches*sizeof(struct machine*));
       for(i = bound_spot; i < bound_structure->numcaches; i++)
         new_top->caches[i - bound_spot] = bound_structure->caches[i];
       new_top->sibling = bound_structure->sibling;
       new_top->children = bound_structure->children;
       new_top->compressed = bound_structure->compressed;
       new_top->cycles = bound_structure->cycles;
       new_top->flops = bound_structure->flops;
       //need to remove misses from inloop that aren't looked at now
       cost = cost_high(new_top, inloop, totalcaches, totalcaches - new_caches);
     }
  }
      //fprintf(stderr, "here\n");
  free(misses);
  return cost;
}

//A high end cost estimate that accounts for what calculations occur in each loop
//Uses the same algorithm as the serial case accounting for extra memory buses
double cost_high(struct machine *inmachine, struct node* inloop, int totalcaches, int startcache) {
  double cost = 0.0;
  double cost2 = 0.0;
  long long* misses;
  int i;
  int junk;
  struct machine *placeholder;
  if(inloop->its == 0)
    return 0;
  misses = sumvarsP(inmachine, inloop, totalcaches, startcache);
  //fprintf(stderr, "children %lld %d\n", inloop->numchildren, totalcaches - startcache);
  //print_misses(inmachine, misses);
  cost = min_loop_costP(inmachine, misses, totalcaches - startcache, &junk, &placeholder);
  free(misses);
  //printf("cost in %lf\n", cost);
  if(cost > 0) {
    for(i = 0; i < inloop->numchildren; i++){
      cost2 += cost_high(inmachine, inloop->children[i], totalcaches, 0);
    //   printf("cost2 in %lf %d\n", cost, i);
    }
    if(cost2 > cost){
        //printf("RET cost2 in %lf\n", cost2);
      return cost2;
    }
  }
  //printf("RET cost in %lf\n", cost);
  return cost;
}

long long* sumvarsP(struct machine *inmachine, struct node* inloop, int caches, int startcache) {
  long long* misses;
  int i, j, k;
  struct machine *ptr;
  int number = 1;
  k = 0;
  ptr = inmachine;
  //fprintf(stderr, "caches %d\n", caches);
  //print_machine(inmachine);
  misses = malloc(caches*sizeof(long long));
  while(k < caches && ptr != NULL) {
      if(ptr->compressed)
	number *= ptr->compressed;
      for(i = 0; i < ptr->numcaches; i++) {
    //printf("here2\n");
	  misses[k] = 0;
	  for(j = 0; j < inloop->variables; j++) {
	    misses[k] += inloop->vars[j]->misses[k+startcache];
	    //fprintf(stderr, "misses %lld\n", misses[k]);
          }
          misses[k] *= number;
      	k++;
	     //fprintf(stderr, "while %lld\n", misses[k]);
    }
    ptr = ptr->children;
  }
  return misses;
}

//calculates which memory structure is more costly for the misses passed in
double min_loop_costP(struct machine *inmachine, long long* misses, int caches, int* bound_struct, struct machine **bound_level) {
  double cost = 0;
  int i, k;
  int buses;
  struct machine *ptr;
  k = 0;
  ptr = inmachine;
  while(k < caches && ptr != NULL) {
    //fprintf(stderr, "test %d\n", ptr->numcaches);
    for(i = 0; i < ptr->numcaches; i++) {
      buses = buses_to_me(ptr, inmachine, i);
      //fprintf(stderr, "buses %d\n", buses);
	//fprintf(stderr, "eval %lf\n", (double)(misses[k]*ptr->caches[i]->linesize)/(double)ptr->caches[i]->bandwidth);
      //fprintf(stderr, "misses %lld k %d\n", misses[k], k);
	if((double)(misses[k]*ptr->caches[i]->linesize) /((double)(ptr->caches[i]->bandwidth*buses)) > cost) {
 //         need something to figure out how many buses connect to the next smaller structure
	  cost = (double)(misses[k]*ptr->caches[i]->linesize)/((double)(ptr->caches[i]->bandwidth*buses));
          (*bound_struct) = i;
          (*bound_level) = ptr;
        }
        k++;
    }
    ptr = ptr->children;
  }
  return cost;
}
