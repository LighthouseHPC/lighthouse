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
  struct node *s1, *s2, *s3;
  struct node *l1, *l2, *l3, *l4;
  struct node **c1, **c2, **c3, **c4;
  struct var **s1vars, **s2vars, **s3vars;
  long long i, n, its;
  struct machine *quadfather;
  struct var *a, *a2, *b, *c, *t, *t2, *t3;
  long long  *varmap;
  double *costs;
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
  //fprintf(stderr, "its %d\n", its);
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
  b = create_var("b\0", iterate2, 1, 0);
  t = create_var("t\0", NULL, 0, 0);
  t2 = create_var("t\0", NULL, 0, 0);
  t3 = create_var("t\0", NULL, 0, 0);
  c = create_var("c\0", iterate2, 1, 0);
  s1vars = (struct var**)malloc(sizeof(struct var*)*3);
  s2vars = (struct var**)malloc(sizeof(struct var*)*3);
  s3vars = (struct var**)malloc(sizeof(struct var*)*1);
  s1vars[0] = a;
  s1vars[1] = b;
  s1vars[2] = t2;
 // s1vars[3] = a2;
 // s1vars[4] = t;
 // s1vars[5] = t3;
//  s1vars[6] = c;
  s2vars[0] = t3;
  s2vars[1] = c;
  s2vars[2] = a2;
  s3vars[0] = t;
  s1 = create_state(s1vars, 3);
  s2 = create_state(s2vars, 3);
  s3 = create_state(s3vars, 1);
  c1 = (struct node**)malloc(sizeof(struct node*)*2);
  c2 = (struct node**)malloc(sizeof(struct node*)*2);
  c3 = (struct node**)malloc(sizeof(struct node*)*3);
  c4 = (struct node**)malloc(sizeof(struct node*)*2);
  c1[0] = s1;
  l1 = create_loop(n, c1, 1, it2);
  c2[0] = s2;
  l2 = create_loop(n, c2, 1, it2);
  c3[0] = s3;
  c3[1] = l1;
  c3[2] = l2;
  l3 = create_loop(n/2, c3, 3, it1);
  c4[0] = l3;
  l4 = create_loop(its, c4, 1, it3);
  quadfather = build_machine();
  fprintf(stderr, "its %d\n", its);
  
  num_misses = all_misses(quadfather, l4);
  fprintf(stderr, "its %d\n", its);
  costs = parallel_costs(quadfather, l4);
  fprintf(stderr, "its %d\n", its);
  print_misses(quadfather, l4);
<<<<<<< .mine
  fprintf(stderr, "its %d\n", its);
  printf("cost %lf %lf\n", costs[0], costs[1]);
=======
  printf("%lf, %lf\n", costs[0], costs[1]);
>>>>>>> .r12502
  return 0;
}
