#ifndef TREE_H
#define TREE_H

struct var{
  char *name;
  char **iterate; //The iterates that access a variable
  long long iterates;
  long long* misses;
};

struct node{
  long long its;  //iterations of a loop.  If this is a statement then it is 0
  struct node **children;
  long long numchildren;
  struct var **vars; //Variables used.  If a variable is used multiple times then it appears more than once
  long long variables;
  char *iterate; //What iterate moves through a loop
};

struct var* create_var(char*, char**, long long);
struct node* create_state(struct var**, long long);
struct node* create_loop(long long, struct node**, long long, char*);
void delete_tree(struct node*);
void delete_node(struct node*);
void delete_nodes(struct node*);
void delete_var(struct var*);
struct node* dot_to_tree(char*);

#endif
