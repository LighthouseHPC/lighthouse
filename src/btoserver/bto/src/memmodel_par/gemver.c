#include <stdlib.h>
#include <stdio.h>
#include <string.h>
extern "C" {
#include "memmodel_par.h"
#include "parallel_machines.h"
#include "cost_par.h"
}
#include "build_machine.h"

int main(int argc, char*argv[]){
  struct node *s1, *s2, *s3, *s4;
  struct node *l1, *l2, *l3, *l4, *l5, *l6, *l7, *l8, *l9;
  struct node **c1, **c2, **c3, **c4, **c5, **c6, **c7, **c8, **c9;
  struct var **s1vars, **s2vars, **s3vars, **s4vars;
  long long i, n, its;
  struct machine *quadfather;
  struct var *a, *a2, *a3, *a4, *b, *c, *d, *d2, *e, *u1, *u2, *v1, *v2;
  long long *varmap;
  double* costs;
  char *it1, *it2, *it3;
  char **iterate, **iterate2;
  long long TLBmiss, L1miss, L2miss;
  long long L1size, L2size, TLBsize;
  long long* num_misses;
  for(i = 0; i < argc; i++){
	if(strcmp(argv[i], "-n") == 0)
	  n = atoi(argv[i+1]);
	if(strcmp(argv[i], "-i") == 0)
	  its = atoi(argv[i+1]);
  }
  it1 = (char*)malloc(sizeof(char)*2);
  it2 = (char*)malloc(sizeof(char)*2);
  it3 = (char*)malloc(sizeof(char)*2);
  it1[0] = 'i';
  it1[1] = '\0';
  it2[0] = 'j';
  it2[1] = '\0';
  it3[0] = 'k';
  it3[1] = '\0';
  iterate = (char**)malloc(sizeof(char*)*2);
  iterate2 = (char**)malloc(sizeof(char*)*1);
  iterate[0] = it1;
  iterate[1] = it2;
  iterate2[0] = it2;
  a = create_var("a\0", iterate, 2, 0);
  a2 = create_var("a\0", iterate, 2, 0);
  a3 = create_var("a\0", iterate, 2, 0);
  a4 = create_var("a\0", iterate, 2, 0);
  b = create_var("b\0", iterate2, 1, 0);
  c = create_var("c\0", iterate, 1, 0);
  d = create_var("d\0", iterate, 1, 0);
  d2 = create_var("d\0", iterate, 1, 0);
  e = create_var("e\0", iterate2, 1, 0);
  u1 = create_var("u1\0", iterate2, 1, 0);
  u2 = create_var("u2\0", iterate2, 1, 0);
  v1 = create_var("v1\0", iterate, 1, 0);
  v2 = create_var("v2\0", iterate, 1, 0);
  s1vars = (struct var**)malloc(sizeof(struct var*)*3);
  s2vars = (struct var**)malloc(sizeof(struct var*)*3);
  s3vars = (struct var**)malloc(sizeof(struct var*)*4);
  s4vars = (struct var**)malloc(sizeof(struct var*)*3);
  s1vars[0] = a;
  s1vars[1] = u1;
  s1vars[2] = v1;
  s2vars[0] = a2;
  s2vars[1] = u2;
  s2vars[2] = v2;
  s3vars[0] = a3;
  s3vars[1] = b;
  s3vars[2] = c;
  s3vars[3] = d;
  s4vars[0] = a4;
  s4vars[1] = d2;
  s4vars[2] = e;
  s1 = create_state(s1vars, 3);
  s2 = create_state(s2vars, 3);
  s3 = create_state(s3vars, 4);
  s4 = create_state(s4vars, 3);
  c1 = (struct node**)malloc(sizeof(struct node*)*2);
  c2 = (struct node**)malloc(sizeof(struct node*)*2);
  c3 = (struct node**)malloc(sizeof(struct node*)*2);
  c4 = (struct node**)malloc(sizeof(struct node*)*2);
  c5 = (struct node**)malloc(sizeof(struct node*)*2);
  c6 = (struct node**)malloc(sizeof(struct node*)*2);
  c7 = (struct node**)malloc(sizeof(struct node*)*2);
  c8 = (struct node**)malloc(sizeof(struct node*)*2);
  c9 = (struct node**)malloc(sizeof(struct node*)*4);
  c1[0] = s1;
  l1 = create_loop(n, c1, 1, it2);
  c2[0] = l1;
  l2 = create_loop(n, c2, 1, it1);
  c3[0] = s2;
  l3 = create_loop(n, c3, 1, it2);
  c4[0] = l3;
  l4 = create_loop(n, c4, 1, it1);
  c5[0] = s3;
  l5 = create_loop(n, c5, 1, it2);
  c6[0] = l5;
  l6 = create_loop(n, c6, 1, it1);
  c7[0] = s4;
  l7 = create_loop(n, c7, 1, it2);
  c8[0] = l7;
  l8 = create_loop(n, c8, 1, it1);
  c9[0] = l2;
  c9[1] = l4;
  c9[2] = l6;
  c9[3] = l8;
  l9 = create_loop(its, c9, 4, it3);
  quadfather = build_machine();
  num_misses = all_misses(quadfather, l9);
  print_misses(quadfather, l9);
  costs = parallel_costs(quadfather, l9);
  printf("cost: %lf %lf\n", costs[0], costs[1]);
  return 0;
}