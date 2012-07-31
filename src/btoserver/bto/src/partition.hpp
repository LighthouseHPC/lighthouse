#ifndef PARTITION_HPP_
#define PARTITION_HPP_

#include "syntax.hpp"
#include <stack>
#include "work.hpp"

//////////////////// HELPERS //////////////////////////////////////////////////
void update_graph(vertex u, graph &g, vector<bool> &up);
void update_mult(vertex u, graph &g, vector<bool> &up);
void check_depth(int curr, int &max_depth, vector<subgraph*> subs);

//////////////////// FIND PARTITIONING ///////////////////////////////////////
void find_partitioning(graph& g, std::stack<work_item>& s,
		  vector<rewrite_fun> const &check, vector<rewrite_fun> const &optimizations, vector<algo> const &algos,
		  vector<rewrite_fun> const &rewrites, bool dot, int min_depth, int max_depth);

//////////////////// PARTITIONERS ////////////////////////////////////////////
bool partition_add(vertex u, graph& g);
bool check_partition_add(vertex u, graph& g);

bool part_mult_left_result(vertex u, graph &g);
bool check_part_mult_left_result(vertex u, graph &g);

bool part_mult_right_result(vertex u, graph &g);
bool check_part_mult_right_result(vertex u, graph &g);

bool part_mult_scl(vertex u, graph &g);
bool check_part_mult_scl(vertex u, graph &g);

bool part_mult_left_right(vertex u, graph &g);
bool check_part_mult_left_right(vertex u, graph &g);

#endif /*PARTITION_HPP_*/
