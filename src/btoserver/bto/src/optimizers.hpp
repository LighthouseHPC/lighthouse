#ifndef OPTIMIZERS_HPP_
#define OPTIMIZERS_HPP_

#include "syntax.hpp"
#include <set>

/////////////////// OPTIMIZATIONS /////////////////////////////////////////////
bool fuse_loops(vertex u, graph& g);
bool check_fuse_loops(vertex u, graph& g);

bool merge_scalars(vertex u, graph& g);
bool check_merge_scalars(vertex u, graph& g);

bool pipeline(vertex u, graph& g);
bool check_pipeline(vertex u, graph& g);


// checkers - need to return a set containing pairs of subgraph id's for which
// the optimization can merge the loops
// optimizers - need to return the id of the subgraph remaining after the merge.  
// This can be a new unique id, or the id of one of the old subgraphs that was merged.
int fuse_loops_new(vertex u, graph& g, std::pair<int,int> &toMerge);
std::set<std::pair<int,int> > check_fuse_loops_new(vertex u, graph& g, std::set<int> &toMerge);

int merge_scalars_new(vertex u, graph& g, std::pair<int,int> &toMerge);
std::set<std::pair<int,int> > check_merge_scalars_new(vertex u, graph& g, std::set<int> &toMerge);

int pipeline_new(vertex u, graph& g, std::pair<int,int> &toMerge);
std::set<std::pair<int,int> > check_pipeline_new(vertex u, graph& g, std::set<int> &toMerge);

// Vectorize inner loops could be an optimization...

#endif /*OPTIMIZERS_HPP_*/
