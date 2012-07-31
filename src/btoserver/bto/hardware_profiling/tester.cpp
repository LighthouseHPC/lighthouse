extern "C" {
  #include "../src/memmodel_par/parallel_machines.h"
}
#include "topology_parse.hpp"
#include <iostream>

int main () {
  struct machine* topo;
  int values[3];
  topo = topology_parse();
  print_machine(topo);
  compress_machine(topo);
  print_machine(topo);
  perculate_up(topo, values);
  print_machine(topo);
  return 0;
}
