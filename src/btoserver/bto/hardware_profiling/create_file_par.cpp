#include <fstream>
#include "create_file_par.hpp"

//creates a file to create a machine with num_caches and sizes and bandwidths passes in
//could be changed in the future to add in associativity and line size as well
void bandwidths_to_file(long long* bandwidths , int num_caches) {
  std::ofstream my_file;
  my_file.open("bandwidths.temp");
  my_file << num_caches << "\n";
  for (int i = 0; i < num_caches; i++)
    my_file << bandwidths[i] << "\n";
  return;
}
