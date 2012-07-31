#ifndef ENUMERATE_HPP_
#define ENUMERATE_HPP_

#include "syntax.hpp"

bool enumerate_loop_merges(std::stack<work_item> &s,
						   work_item &current,
						   vector<optim_fun_chk> const &checks,
						   vector<optim_fun> const &optimizations,
						   vector<rewrite_fun> & rewrites,
						   std::vector<algo> &algos);

void get_current_subgraphs(graph &g, int nesting, vector<subgraph*> & subs);

void enumerate(std::map<int,std::set<int> > common_itr,
		std::map<int,std::set<int> > &depend,
		std::vector<std::set<int>* > &mergable);

void map_subgraphs(int map_from, subgraph* map_to, 
		std::map<int, int> &sg_map);

void evaluate_edges(int curr, int curr_parent, subgraph *working,
		std::vector<subgraph*> &sgs, std::map<int, int> &sg_map, 
		graph &g, std::map<int,std::set<int> > &depend);

void generate_work_items(graph &g, std::vector<std::set<int>* > &mergable,
						  std::vector<vertex> &workingVertices,
						  vector<optim_fun_chk> const &checks,
						  vector<optim_fun> const &optimizations,
						  std::stack<work_item> &s,
						  work_item &old_wi,
						  vector<rewrite_fun> & rewrites,
						  std::vector<algo> &algos);

#endif /*ENUMERATE_HPP_*/


