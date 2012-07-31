#include "tree.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

//Creates a variable
struct var* create_var(char *name, char **iterate, long long iterates){
  struct var *ret;
  int i;
  ret = malloc(sizeof(struct var));
  ret->name = name;
  ret->iterate = iterate;
  ret->iterates = iterates;
  ret->misses = 0;
  return ret;
}; 

//create a loop
struct node* create_loop(long long its, struct node **children, long long numchildren, char* iterate){
  struct node *ret;
  int i, j, k = 0;
  ret = malloc(sizeof(struct node));
  ret->numchildren = numchildren;
  ret->children = children;
  ret->its = its;
  ret->iterate = iterate;
  ret->variables = 0;
  for(i = 0; i < numchildren; i++) //Find the number of variable accessed by the loop
    ret->variables += children[i]->variables;
  ret->vars = malloc(sizeof(struct var*)*ret->variables);
  for(i = 0; i < numchildren; i++)  //Find the actual variables accessed by the loop
    for(j = 0; j < children[i]->variables; j++) {
      ret->vars[k] = children[i]->vars[j];
      k++;
    }
  return ret;
}

//Create a statement
struct node* create_state(struct var **vars, long long num){
  struct node *ret;
  ret = malloc(sizeof(struct node));
  ret->its = 0;
  ret->numchildren = 0;
  ret->children = NULL;
  ret->iterate = NULL;
  ret->vars = vars;
  ret->variables = num;
  return ret;
};

void delete_tree(struct node* in) {
 int i;
 for(i = 0; i < in->variables; i++)
   delete_var(in->vars[i]);
 delete_nodes(in);
 return;
}

void delete_nodes(struct node* in) {
  int i;
  if(in->its == 0) {
    delete_node(in);
    return;
  }
  for(i = 0; i < in->numchildren; i++)
    delete_nodes(in->children[i]);
  delete_node(in);
  return;
}
void delete_node(struct node* in) {
  free(in->vars);
  free(in->iterate);
  free(in->children);
  free(in);
  return;
}

void delete_var(struct var* in) {
  int i;
  free(in->misses);
  free(in->name);
  for(i = 0; i < in->iterates; i++)
    free(in->iterate[i]);
  free(in->iterate);
  free(in);
#ifdef SYMBOLIC
  for(i = 0; i < 100; i++)
    free(in->misses[i])
  free(in->misses);
#endif
  return;
}

/*struct node* dot_to_tree(char *infile) {
  struct node *root;
  FILE *fp;
  char line[1000];
  int i, vars = 0;
  int j, k;
  int children = 0;
  int iterates = 0;
  char itnames[10][10];
  char iterate[10];
  char varname[10];
  struct var **childvars;
  if((fp = fopen(infile, "r")) == NULL) {
    printf("Cannot open file.\n");
    exit(1);
  }
  fgets ( line, sizeof(line), fp );
  fgets ( line, sizeof(line), fp );
  i = 0;
  while(line[i] != ':')
    i++;
  i++;
  iterate[0] = line[i];
  iterate[1] = '\0';
  while(line[i] != ':')
    i++;
  i++;
  while(line[i] != ':')
    i++;
  i++;
  while(line[i] != ';') {
    if(children == 0)
      children = atoi(line[i]);
    else
      children = children * 10 + atoi(line);
    i++;
  }  
  allocate pointers to children
  while(line[i] != ':')
    i++;
  i++;
  while(line[i] != '"') {
    if(vars == 0)
      vars = atoi(line[i]);
    else
      vars = vars * 10 + atoi(line);
    i++;
  } 
  childvars = malloc(vars*sizeof(struct var*));
  for(i = 0; i < vars; i++) {
    j = 0;
    fgets ( line, sizeof(line), fp );
    fgets ( line, sizeof(line), fp );
    while(line[j] != '"')
      j++;
    j++;
   k = 0;
   while(line[j] != ';') {
     name[k] = line[j];
     j++;
     k++;
   }
   name[k] = '\0';
   j += 2;
   iterates = atoi(line[j]);
   j += 2;
   for(k = 0; k < iterates; k++) {
     iterate[k][0] = line[j];
     iterate[k][1] = '\0';
     j += 2;
   }
   childvars[i] = create_var(name, iterate, iterates);
  }
  root = struct node* create_loop(1, pointers to children, numchildren, iterate);
  while ( fgets ( line, sizeof(line), fp ) != NULL ) {
  }
  if(fclose( fp )) 
    printf("File close error.\n");
  root = malloc(sizeof(struct node));

  return root;
}*/
