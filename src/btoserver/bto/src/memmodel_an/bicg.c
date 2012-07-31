#include <stdlib.h>
#include <stdio.h>
#include "memmodel_clean.h"
#include "cost.h"

int main(int argc, char *argv[]){
  struct node *s1, *s2;
  struct node *l1, *l2, *l3;
  struct node **c1, **c2, **c3;;
  struct var **s1vars, **s2vars;
  long long i;
  long long n, its;
  double cost;
  long long TLBmiss, L1miss, L2miss;
  struct var *a, *a2, *b, *c, *d, *e;
  char *it1, *it2, *it3;
  char **iterate, **iterate2;
  struct machine* quadfather;
  long long* num_misses;
  for(i = 0; i < argc; i++){
	if(strcmp(argv[i], "-n") == 0)
	  n = atoi(argv[i+1]);
	if(strcmp(argv[i], "-i") == 0)
	  its = atoi(argv[i+1]);
  }
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
  b = create_var("b\0", iterate, 1);
  e = create_var("e\0", iterate, 1);
  c = create_var("c\0", iterate2, 1);
  d = create_var("d\0", iterate2, 1);
  s1vars = malloc(sizeof(struct var*)*3);
  s2vars = malloc(sizeof(struct var*)*3);
  s1vars[0] = a;
  s1vars[1] = b;
  s1vars[2] = c;
  s2vars[0] = a2;
  s2vars[1] = d;
  s2vars[2] = e;
  s1 = create_state(s1vars, 3);
  s2 = create_state(s2vars, 3);
  c1 = malloc(sizeof(struct node*)*2);
  c2 = malloc(sizeof(struct node*)*2);
  c3 = malloc(sizeof(struct node*)*2);
  c1[0] = s1;
  c1[1] = s2;
  l1 = create_loop(n, c1, 2, it2);
  c2[0] = l1;
  l2 = create_loop(n, c2, 1, it1);
  c3[0] = l2;
  l3 = create_loop(its, c3, 1, it3);
  quadfather = create_quadfather();
  num_misses = all_misses(quadfather, l3);
  print_misses(quadfather, num_misses);
  cost = new_cost(quadfather, l3);
  printf("cost %lf\n", cost);
  //TLBmiss = mem_misses(l3, 100);
  //L1miss = mem_misses(l3, 1700);
  //L2miss = mem_misses(l3, 10000);
  //printf("%ld\n", L2miss);
  //L2miss = mem_misses(l3, 90000);
  //printf("%ld\n", TLBmiss);
  //TLBmiss = mem_misses(l3, 30);
  //printf("%ld\n", L1miss);
  //printf("%ld\n", L2miss);
  //printf("%ld\n", TLBmiss);
  //TLBmiss = mem_misses(l3, 10);
  //printf("%ld\n", TLBmiss);
  //printf("%ld\n", s1->DA1);
  //printf("%ld\n", s1->DAall);
  //printf("%ld\n", l1->DA1);
  //printf("%ld\n", l1->DAall);
  //printf("%ld\n", l2->DA1);
  //printf("%ld\n", l2->DAall);
  //printf("%ld\n", l3->DA1);
  //printf("%ld\n", l3->DAall);
  //printf("%ld\n", s1->WS1);
  //printf("%ld\n", s1->WSall);
  //printf("%ld\n", s2->WS1);
  //printf("%ld\n", s2->WSall);
  //printf("%ld\n", l1->WS1);
  //printf("%ld\n", l1->WSall);
  //printf("%ld here\n", l2->WS1);
  //printf("%ld\n", l2->WSall);
  //printf("%ld\n", l3->WS1);
  //printf("%ld\n", l3->WSall);
  //printf("%ld\n", s1->variables);
  //printf("%ld\n", l1->variables);
  //printf("%ld\n", l2->variables);
  //printf("%ld\n", l2->children[0]->WSall);
  return 0;
}
