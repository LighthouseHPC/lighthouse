#include <stdlib.h>
#include <stdio.h>
#include "memmodel_clean.h"
#include "cost.h"

int main(int argc, char*argv[]){
  struct node *s1, *s2, *s3;
  struct node *l1, *l2, *l3, *l4;
  struct node **c1, **c2, **c3, **c4;
  struct var **s1vars, **s2vars, **s3vars;
  long long i, n, its;
  struct machine *quadfather;
  struct var *a, *a2, *b, *c, *t, *t2, *t3;
  long long  *varmap;
  double cost;
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
  it1 = malloc(sizeof(char)*2);
  it2 = malloc(sizeof(char)*2);
  it3 = malloc(sizeof(char)*2);
  it1[0] = 'i';
  it1[1] = '\0';
  it2[0] = 'j';
  it2[1] = '\0';
  it3[0] = 'k';
  it3[1] = '\0';
  iterate = malloc(sizeof(char*)*2);
  iterate2 = malloc(sizeof(char*)*1);
  iterate[0] = it1;
  iterate[1] = it2;
  iterate2[0] = it2;
  a = create_var("a\0", iterate, 2);
  a2 = create_var("a\0", iterate, 2);
  b = create_var("b\0", iterate2, 1);
  t = create_var("t\0", NULL, 0);
  t2 = create_var("t\0", NULL, 0);
  t3 = create_var("t\0", NULL, 0);
  c = create_var("c\0", iterate2, 1);
  s1vars = malloc(sizeof(struct var*)*3);
  s2vars = malloc(sizeof(struct var*)*3);
  s3vars = malloc(sizeof(struct var*)*1);
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
  c1 = malloc(sizeof(struct node*)*2);
  c2 = malloc(sizeof(struct node*)*2);
  c3 = malloc(sizeof(struct node*)*3);
  c4 = malloc(sizeof(struct node*)*2);
  //fprintf(stderr, "its %d\n", its);
  c1[0] = s1;
  l1 = create_loop(n, c1, 1, it2);
  c2[0] = s2;
  l2 = create_loop(n, c2, 1, it2);
  c3[0] = s3;
  c3[1] = l1;
  c3[2] = l2;
  l3 = create_loop(n, c3, 3, it1);
  c4[0] = l3;
  l4 = create_loop(its, c4, 1, it3);
  quadfather = create_quadfather();
  num_misses = all_misses(quadfather, l4);
  cost = new_cost(quadfather, l4);
  print_misses(quadfather, num_misses);
  printf("cost %lf\n", cost);
//  TLBmiss = mem_misses(l4, 100);
//  L1miss = mem_misses(l4, 1700);
//  L2miss = mem_misses(l4, 10000);
//  printf("%d\n", TLBmiss);
//  printf("%d\n", L1miss);
//  printf("%d\n", L2miss);
//  L2miss = mem_misses(l4, 23);
//  printf("%d\n", L2miss);
//  L2miss = mem_misses(l4, 5);
//  printf("%d\n", L2miss);
//  printf("%d\n", s1->DA1);
//  printf("%d\n", s1->DAall);
//  printf("%d\n", l1->DA1);
//  printf("%d\n", l1->DAall);
//  printf("%d\n", l2->DA1);
//  printf("%d\n", l2->DAall);
//  printf("%d\n", l3->DA1);
//  printf("%d\n", l3->DAall);
//  printf("%d\n", s1->WS1);
//  printf("%d\n", s1->WSall);
//  printf("%d\n", s2->WS1);
//  printf("%d\n", s2->WSall);
//  printf("%d\n", l1->WS1);
//  printf("%d\n", l1->WSall);
//  printf("%d here\n", l2->WS1);
//  printf("%d\n", l2->WSall);
//  printf("%d\n", l3->WS1);
//  printf("%d\n", l3->WSall);
//  printf("%d\n", s1->variables);
//  printf("%d\n", l1->variables);
//  printf("%d\n", l2->variables);
//  printf("%d\n", l2->children[0]->WSall);
  return 0;
}
