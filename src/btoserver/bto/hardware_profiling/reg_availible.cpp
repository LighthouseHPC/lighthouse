#include <iostream>
#include <fstream>
#include <string.h>
#include <stdlib.h>
#include "reg_availible.hpp"

int reg_avail(int chatter) {
  int regs = 0;
  int difference_found = 0;
  char filename[20];
  char command[100];
  std::ofstream my_file;
  std::ifstream my_file2;
  int cur;
  int last = -1;
  int repeat = 0;
  char linein[100];
  while(difference_found == 0 && regs < 128) {
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
    sprintf(command, "gcc -m32 -O0 -S test%d.c", regs);
    if(chatter > 2)
      fprintf(stderr, "%s\n", command);
    int i = system(command);
    sprintf(command, "grep 3456789 test%d.s > temp%d.out", regs, regs);
    i = system(command);
    cur = 0;
    sprintf(filename, "temp%d.out", regs);
    if(chatter > 2)
      fprintf(stderr, "%s\n", filename);
    my_file2.open (filename);
    if (my_file2.is_open())
      if(chatter > 2)
        fprintf(stderr, "i:%d\n", i);
    my_file2.getline(linein, 100);
    while(!my_file2.eof()) {
      if(chatter > 2)
        fprintf(stderr, "%s\n", linein);
      cur++;
      for(int j = 0; j < strlen(linein); j++)
        if(linein[j] == '(') {
          cur--;
          break;
        }
      if(chatter > 2)
        fprintf(stderr, "cur: %d\n", cur);
      my_file2.getline(linein, 100);
    }
    my_file2.close();
    if(cur == last) {
      if(repeat)
        return cur;
      else
        repeat = 1;
    }
    else
      repeat = 0;
    last = cur;
  }
  return regs;
}
