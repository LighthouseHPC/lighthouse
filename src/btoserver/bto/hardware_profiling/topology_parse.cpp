#include <fstream>
#include <stdio.h>
#include <iostream>
#include <string.h>
#include <stdlib.h>
extern "C"{
  #include "../src/memmodel_par/parallel_machines.h"
}

struct machine* topology_parse(char *pathToTop) {
	// pathToTop requires / at end.  as in path/
  
  char inbuffer[2048];
  struct machine* topology;
  struct machine* cur;
  char **caches;
  long long *sizes;
  int cores, threads, sockets;
  int num_caches;
  char delim[2] = "+";
  char *tok[20];
  char *num;
  char *place;
  char *test;
  int last = -1;
  caches = (char**)malloc(10*sizeof(char*));
  sizes = (long long*)malloc(10*sizeof(long long));
  char *machinePath = (char*)malloc(sizeof(char)*(strlen(pathToTop)+
							strlen("hardware_profiling/machine.temp")+1));
  strcpy(machinePath,pathToTop);
  strcat(machinePath,"hardware_profiling/machine.temp");
  std::ifstream in(machinePath);
  free(machinePath);
  if (in.fail()) {
	printf("Failed to open machine.temp\n");
  }
  while(!in.getline(inbuffer, 2048).eof() && in.good()) {
    //std::cout << inbuffer << "\n";
    int j = 0;
    int toks = 0;
    cores = 0;
    threads = 0;
    sockets = 0;
    num_caches = 0;
    while(inbuffer[j] == ' ')
      j++;
  //  std::cout << j << " " << last << "\n";
    tok[0] = strtok(inbuffer, delim);
    while(tok[toks] != NULL) {
      toks++;
      tok[toks] = strtok(NULL, delim);
    }
    for(int i = 0; i < toks; i++) {
      if(strstr(tok[i], "Core"))
	cores++;
      if(strstr(tok[i], "Socket"))
        sockets++;
      if(strstr(tok[i], "P#"))
  	threads++;
      if(strstr(tok[i], "PU"))
  	threads++;
      if(strstr(tok[i], "L")) {
        if(strstr(tok[i], "KB") != NULL) {
          place = strtok(tok[i], "(\0");
          caches[num_caches] = (char*)malloc((3)*sizeof(char));
	  strcpy(caches[num_caches], "L0");// = strtok(tok, "(");
          place = strtok(NULL, "(\0"); 
          num = strpbrk(place, "0123456789");
	  sizes[num_caches] = atoi(num);
          sizes[num_caches] *= 1024;
	  if(sizes[num_caches] != 0)
	    num_caches++;
        }
      if(strstr(tok[i], "MB") != NULL) {
        place = strtok(tok[i], "(\0");
        caches[num_caches] = (char*)malloc((strlen(place)+1)*sizeof(char));
	strcpy(caches[num_caches], place);// = strtok(tok, "(");
        place = strtok(NULL, "(\0"); 
        num = strpbrk(place, "0123456789");
        sizes[num_caches] = atoi(num);
        sizes[num_caches] *= 1024*1024;
	if(sizes[num_caches] != 0)
	{
          //std::cout << caches[num_caches] << std::endl;
          num_caches++;
        }
      }
      if(strstr(tok[i], "GB") != NULL) {
        place = strtok(tok[i], "(\0");
        caches[num_caches] = (char*)malloc((3)*sizeof(char));
	strcpy(caches[num_caches], "L2");// = strtok(tok, "(");
        place = strtok(NULL, "(\0"); 
        num = strpbrk(place, "0123456789");
        sizes[num_caches] = atoi(num);
	sizes[num_caches] *= 1024*1024*1024;
	if(sizes[num_caches] != 0)
          num_caches++;
      }
    }
      /*f(num_caches != 0)
        if(sizes[num_caches-1] > 128*1024*1024 && num_caches)
	  num_caches--;*/
    }  
    if(last == -1) {
      //create top level
      char name[10];
      strcpy(name, "machine");
      cur = new_machine(num_caches, caches, sizes, cores, threads, sockets, name);
      topology = cur;
    }
    else if(last == j) {
      //add another child
      cur = add_sibling(cur, num_caches, caches, sizes, cores, threads, sockets);
//	std::cout << "sib\n";
    }
    else if(j > last) {
      //child has a child
      cur = add_child(cur, num_caches, caches, sizes, cores, threads, sockets);
//	std::cout << "child\n";
    }
    else {
      //find place to add new child
      cur = topology;
      for(int k = 0; k < (j/2); k++) {
        cur = cur->children;
        while(cur->sibling != NULL)
	  cur = cur->sibling;
      }
      cur = add_sibling(cur, num_caches, caches, sizes, cores, threads, sockets);
    }
    last = j;
  }
  in.close();
  if(last == -1) {
    fprintf(stderr, "No machine topology found\n");
    exit(1);
  }
  return topology;
}
