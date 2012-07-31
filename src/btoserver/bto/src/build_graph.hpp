#ifndef BUILD_GRAPH_HPP
#define BUILD_GRAPH_HPP

#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include "syntax.hpp"

void
program2graph(vector<stmt*> const& p, 
	      map<string,type*>& inputs, 
	      map<string,type*>& inouts, 
	      map<string,type*>& outputs, 
	      graph& g);

void print_graph(std::ostream& out, graph const& g);

void update_sizes(graph &g);
void update_dim_info(graph &g);

#endif  // BUILD_GRAPH_HPP
