#include <iostream>
#include <fstream>
#include <string>
#include <stdlib.h>
#include <string.h>
#include "vector_reg_availible.hpp"

int vector_reg_avail() {
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
    sprintf(filename, "testv%d.c", regs);
    my_file.open(filename);
    my_file << "#include <xmmintrin.h>\n\n";
    my_file << "__m128d double_(double *a) {\n";
    for(int i = 0; i < regs +1; i++)
      my_file << "__m128d a" << i << ";\n";
    for (int i = 0; i < regs+1; i++)
      my_file << "  a" << i << " = " << "_mm_loadu_pd(a+" << i*2 << ");\n";
    for (int i = 0; i < regs; i++)
      my_file << "  a0 = _mm_add_pd(a0,a" << i+1<<");\n";
    my_file << "  return a0;\n";
    my_file << "}";
    my_file.close();
    sprintf(command, "gcc -m32 -msse -O3 -S testv%d.c", regs);
    fprintf(stderr, "%s\n", command);
    int i = system(command);
    sprintf(command, "grep add testv%d.s > temp%d.out", regs, regs);
    i = system(command);
    cur = 0;
    sprintf(filename, "temp%d.out", regs);
    fprintf(stderr, "%s\n", filename);
    my_file2.open (filename);
    if (my_file2.is_open())
      fprintf(stderr, "i:%d\n", i);
    my_file2.getline(linein, 100);
    while(!my_file2.eof()) {
      fprintf(stderr, "%s\n", linein);
      cur++;
      for(int j = 0; j < strlen(linein); j++)
        if(linein[j] == '(') {
          cur--;
          break;
        }
      fprintf(stderr, "cur: %d\n", cur);
      my_file2.getline(linein, 100);
    }
    my_file2.close();
    if(cur == last) {
      if(repeat) {
        cur++;
      fprintf(stderr, "return cur: %d\n", cur);
        return cur;
      }
      else
        repeat = 1;
    }
    else
      repeat = 0;
    last = cur;
  }
  return regs;
}
