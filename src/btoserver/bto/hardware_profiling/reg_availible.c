#include <iostream>
#include <fstream>
#include <stdlib.h>
#include "reg_availible.hpp"

int reg_avail() {
  int regs = 0;
  int difference_found = 0;
  char filename[20];
  char command[100];
  std::ofstream my_file;
  while(difference_found == 0 && regs < 8) {
    regs++;
    sprintf(filename, "test%d.c", regs);
    my_file.open(filename);
    my_file << "main() {\n";
    for(int i = 0; i < regs; i++)
      my_file << " register int a" << i << ";\n";
    for (int i = 0; i < regs; i++)
      my_file << "  a" << i << " = " << i+1 << 3456789 << ";\n";
    my_file << "  a0 = a0";
    for (int i = 1; i < regs; i++)
      my_file << " + " << "a" << i;
    my_file << ";\n";
    my_file << "  return a0;\n";
    my_file << "}";
    my_file.close();
    sprintf(command, "gcc -S test%d.c", regs);
    fprintf(stderr, "%s\n", command);
    int i = system(command);
    fprintf(stderr, "%d\n", i);
  }
  return regs;
}
