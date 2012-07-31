extern "C" {
#include "../src/memmodel_par/parallel_machines.h"
}
#include "topology_parse.hpp"
#include "bandwidth.hpp"
#include "create_file.hpp"
#include "create_file_par.hpp"
#include <fstream>
#include "reg_availible.hpp"
#include "vector_reg_availible.hpp"
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <iostream>
#include <unistd.h>
namespace std {
  extern ostream cerr;
}

int main (int argc, char **argv) {
	// determine location of bto (top level directory)
	char* glblPath = getcwd(NULL,0);	
	char *loc = strstr(glblPath,"bto");
	if (loc == NULL) {
		printf("Unable to determine path to bto\n");
		if (glblPath)
			free(glblPath);
		glblPath = (char*)malloc(sizeof(char)*200);
		while (1) {
			printf("Please enter the path or 'quit' to exit\n");
			if (scanf("%s",glblPath) == 0)
				continue;
			if (strstr(glblPath,"quit") == glblPath)
				return 0;
			
			loc = strstr(glblPath,"bto");
			if (loc != NULL)
				break;
		}
		while (getchar() != '\n');
	}

	// default chatter to 0, but look in first argument for valid number
	int chatter = 1;
	if (argc > 1) {
		char *endPtr;
		chatter = (int)strtol(argv[1],&endPtr,10);
		if (argv[1] == endPtr) {
			// no good
			chatter = 1;
		}
	}
	
	int len = loc - glblPath;
	char *topPath = (char*)malloc(sizeof(char)*(len+5));
	strncpy(topPath,glblPath,len+3);
	topPath[len+3] = '/';
	topPath[len+4] = '\0';
	free(glblPath);
	struct machine* topo;
	int values[3];
	int caches;
	long long* bandwidths;
	topo = topology_parse(topPath);
	if(topo == NULL) {
	  std::cerr << "Could not read machine topology from hwloc.\n";
	  exit(1);
        }
        if(chatter) {
	  std::cerr << "Machine topology determined: caches and locations found\n";
	  print_machine(topo);
        }
	compress_machine(topo);
	perculate_up(topo, values);
        if(chatter > 1) {
	  std::cerr << "Machine charactoristics:\n";
	  std::cerr << "Cores: "<< topo->cores << std::endl;
          struct machine *ptr = topo;
	  std::cerr << "Memory structures: \n";
	  while(ptr != NULL) {
	    for(int i = 0; i < ptr->numcaches; i++)
	      std::cerr << ptr->caches[i]->name << " " << ptr->caches[i]->size << " bytes\n";
	    ptr = ptr->children;
	  }
        }
        if(chatter)
          std::cerr << "Determining Macine Bandwidths\n"; 
	bandwidths = par_bandwidths(topo, &caches);
        if(bandwidths == NULL) {
	   std::cerr << "Could not determine bandwidths.\n";
           exit(1);
        }
        if(chatter)
          std::cerr << "Bandwidths Found\n";        
        if(chatter > 1) {
	  for(int i = 0; i < caches -1; i++)
	    std::cerr << "L" << i + 1 << " " << bandwidths[i] << std::endl;
	  std::cerr << "Memory " << bandwidths[caches -1] << std::endl;
 	}
	//print_machine(topo);
	bandwidths_to_file(bandwidths, caches);
        if(chatter)
          std::cerr << "Determining Macine Registers\n";            
	int regs = reg_avail(chatter);
        if(chatter == 1)
          std::cerr << "Number of Registers Found\n"; 
	if(chatter > 1)
          std::cerr << "Number of Registers Found = " << regs << "\n"; 
	std::ofstream my_file;
	my_file.open("regs.temp");
	my_file << regs << "\n";
	my_file.close();
	//int vec_regs = vector_reg_avail();
	//my_file.open("vec_regs.temp");
	//my_file << vec_regs << "\n";
	//my_file.close();
	int sizes[caches];
	struct machine *ptr = topo;
        int j = 0;
        while(ptr != NULL) {
          for(int i = 0; i < ptr->numcaches; i++) {
            sizes[j] = ptr->caches[i]->size;
	    j++;
          }
          ptr = ptr->children;
        }
	create_file(sizes, bandwidths, caches -1, regs, 0);
	std::cerr << "Hardware Profiling Done\n";
	free(topPath);
	return 0;
}
