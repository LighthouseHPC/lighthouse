#ifndef ANALYZE_GRAPH_HPP
#define ANALYZE_GRAPH_HPP

#include <vector>
#include "syntax.hpp"

void init_algos(std::vector<algo>& algos);
void assign_algorithms(vector<algo> const& algos, graph& g);
void assign_work(vector<algo> const& algos, graph& g);
void reintroduce_variables(graph& g);
void initial_lower(graph &g, vector<algo> const &algos, vector<rewrite_fun> const &rewrites);
void lower_graph(vector<algo> const& algos, graph& g);
void verify_parents(graph& g);
void apply_rewrites(vector<rewrite_fun>const& rewrites, graph& g);
std::vector<unsigned int> find_matches(vertex u, graph &g, vector<algo> const& algos);
void apply_match(unsigned int i, vertex u, graph &g, vector<algo> const& algos);

/////////////////// REWRITERS - ALWAYS PERFORM /////////////////////////////////
bool flip_transpose(vertex u, graph& g);
bool flip_transpose_stride(vertex u, graph& g);
bool remove_scalar_transpose(vertex u, graph& g);
bool merge_tmp_output(vertex u, graph& g);
bool remove_intermediate_temporary(vertex u, graph& g);
bool merge_gets(vertex u, graph& g);
bool merge_sumto_store(vertex u, graph &g);
bool move_temporary(vertex u, graph &g);

//////////////////// UNUSED ///////////////////////////////////////////////////
bool lower_stores(vertex u, graph& g);
bool merge_stores(vertex u, graph& g);
bool remove_deadends(vertex u, graph& g);
bool remove_useless_store(vertex u, graph& g);


#endif  // ANALYZE_GRAPH_HPP
