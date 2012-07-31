#include <fstream>
#include "create_file.hpp"

//creates a file to create a machine with num_caches and sizes and bandwidths passes in
//could be changed in the future to add in associativity and line size as well
void create_file(int* cache_sizes, long long* bandwidths, int num_caches, int regs, int vec_regs) {
  std::ofstream my_file;
  if(regs)
    num_caches++;
  if(vec_regs) 
    num_caches++;
  my_file.open("../src/memmodel_an/your_machine.c");
  my_file << "#include \"your_machine.h\"\n";
  my_file << "#include \"machines.h\"\n";
  my_file << "#include <stdlib.h>\n";
  my_file << "#include <string.h>\n";
  my_file << "#include <stdio.h>\n\n";
  my_file << "struct machine* your_machine(){\n";
  my_file << "  struct machine * ret;\n";
  my_file << "  ret = malloc(sizeof(struct machine));\n";
  my_file << "  ret->name = malloc(sizeof(char)*12);\n";
  my_file << "  strcpy(ret->name, \"YourMachine\\0\");\n";
  my_file << "  ret->numcaches = " << num_caches << ";\n";
  my_file << "  ret->caches = malloc(sizeof(struct caches*)*" << num_caches << ");\n";
  if(regs)
     num_caches--;
  if(vec_regs) 
     num_caches--;
  for (int i = 0; i < num_caches; i++)
    my_file << "  ret->caches[" << i << "] = create_cache(" << cache_sizes[i] << ", 64, 8, \"L" << i+1 << "\\0\", " << bandwidths[num_caches-i] << ");\n";
  if(regs) {
    my_file << "  ret->caches[" << num_caches << "] = create_cache(" << regs*8 << ", 64, 8, \"Reg" << "\\0\", " << bandwidths[0] << ");\n";
    num_caches++;
  }
  if(vec_regs) {
    my_file << "  ret->caches[" << num_caches << "] = create_cache(" << vec_regs*16 << ", 64, 8, \"Vec_Reg" << "\\0\", " << bandwidths[0] << ");\n";
  }
  my_file << "  return ret;\n";
  my_file << "}";
  return;
}
