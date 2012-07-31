#include <fstream>
#include "boost/lexical_cast.hpp"
#include "optimizers.hpp"
#include "build_graph.hpp"
#include <string>
#include "type_analysis.hpp"
#include "translate_utils.hpp"

//////////////////////////////////// OPTIMIZER HELPERS ////////////////////////////

bool in (vertex id, std::vector<vertex> &s) {
	std::vector<vertex>::iterator i = s.begin();
	for (; i != s.end(); i++)
		if (*i == id)
			return true;
	
	return false;
}

void compute_reachable(vertex u, graph const& g, vector<bool>& mark)
{
	mark[u] = true;
	for (unsigned int i = 0; i != g.adj(u).size(); ++i) {
		vertex v = g.adj(u)[i];
		if (! mark[v])
			compute_reachable(v, g, mark);
	}
}

bool check_reachable(vertex u, graph &g, vector<vertex> &check,
					 vertex skip, int hops, subgraph *parent) {
	// return true any path out of u can get to any vertex in check
	// false otherwise with certain exceptions
	// 1. it is the path that pipeline is checking in the first place
	// 2. is is another path that could be pipelined and will clean
	//		up after the unrelated pipeline is executed.
	
	for (int i = 0; i != g.adj(u).size(); ++i) {
		vertex v = g.adj(u)[i];
		
		// only want to follow out edges
		if (g.find_parent(v) == parent)
			continue;
		// if v is the value we are testing for pipeline
		// we can not fail because of it
		if (v == skip)
			continue;
		// if we have found another path back to the subgraph
		// there are two options.
		// 1. Something very close, another pipeable data dependance
		//		exists but is ok because that will clean up in
		// 2. The path is long, and unless other subgraphs in between
		//		are fused first, there is a true data dependance.
		if (in(v, check)) {
			if (hops <= 2)
				continue;
			else {
				//std::cout << "\t" << hops<< "\t" << v << "\n";
				return true;
			}
		}
		
		// keep following each path
		if (check_reachable(v, g, check, skip, hops+1,parent))
			return true;
	}
	return false;
}

// sg1 depends on sg2 if sg1 uses stuff computed by sg2
bool depends_on(subgraph* sg1, subgraph* sg2, graph const& g)
{
	vector<bool> mark(g.num_vertices(), false);
	for (unsigned int i = 0; i != sg2->vertices.size(); ++i) {
		vertex u = sg2->vertices[i];
		compute_reachable(u, g, mark);
	}
	for (unsigned int i = 0; i != sg1->vertices.size(); ++i) {
		vertex u = sg1->vertices[i];
		if (mark[u])
			return true;
	}
	return false;
}

int useless_temporary(vertex u, graph& g) {
	if ((g.info(u).op == temporary
			|| g.info(u).op == output
			|| g.info(u).op == store_column
		    || g.info(u).op == store_add_column
			|| g.info(u).op == store_row
			|| g.info(u).op == store_add_row
			|| g.info(u).op == store_element
			|| g.info(u).op == store_add_element)
			//&& g.info(u).eval == defer // if flagged as evaluate, could be an output variable!
			&& g.adj(u).size() >= 1 && g.inv_adj(u).size() == 1) {
		
		int prev = g.inv_adj(u)[0];
		for (int i = 0; i != g.adj(u).size(); i++) {
			int succ = g.adj(u)[i];
			if (g.info(u).t.t) {
				if ((g.info(u).t.t->k == column && g.info(prev).t.k == column
						&& (g.info(succ).op == get_column 
							|| g.info(succ).op == get_column_from_row))
					|| (g.info(u).t.t->k == row && g.info(prev).t.k == row
						&& (g.info(succ).op == get_row
							|| g.info(succ).op == get_row_from_column))
					|| ((g.info(u).t.k == row || g.info(u).t.k == column)
						&& g.info(prev).t.k == scalar
						&& g.info(succ).op == get_element)) {
					return i;
				}
			}
			else {
				if ((g.info(u).t.k == row || g.info(u).t.k == column)
						&& g.info(prev).t.k == scalar
						&& g.info(succ).op == get_element) {
					return i;
				}
			}
		}
		return -1;
	} 
	else
		return -1;
}

/////////////////////////////////END OPTIMIZER HELPERS ////////////////////////////

//////////////////////////////////// OPTIMIZERS ///////////////////////////////////
/*
/// given
///      A.0
///   /       \
/// A.1(i)   A.2(i)
/// look from A.0 at A.1 and A.2 to see if they get the same data
/// are unmerged, and their graphs are possitioned for legal fusion

bool check_fuse_loops(vertex u, graph& g)
{
	std::vector<std::pair<vertex,vertex> > compat;
	for (unsigned int i = 0; i < g.adj(u).size(); ++i) {
		vertex iv = g.adj(u)[i];
		if (g.info(iv).op == get_row || g.info(iv).op == get_column 
			|| g.info(iv).op == get_row_from_column 
			|| g.info(iv).op == get_column_from_row
			|| g.info(iv).op == get_element) {
			for (unsigned int j = i + 1; j < g.adj(u).size(); ++j) {
				vertex jv = g.adj(u)[j];
				// Already fused
				if (g.find_parent(jv) == g.find_parent(iv))
					continue;
				if (g.info(jv).op == g.info(iv).op
						&& (! (depends_on(g.find_parent(iv), g.find_parent(jv), g)
						|| depends_on(g.find_parent(jv), g.find_parent(iv), g)))) {
					// g.merge requires subgraphs to be contained within same subgraph
					if (g.find_parent(jv)->parent == g.find_parent(iv)->parent) {
						compat.push_back(std::pair<vertex,vertex> (iv,jv));
					}
				}//if
			}//for
		}//if
	}//for
	
	//for (int i =0; i != compat.size(); ++i) {
	//	std::cout << "look at me " << compat[i].first << "\t" << compat[i].second << "\n";
	//}
	
	if (compat.size() != 0)
		return true;
	return false;
}

bool fuse_loops(vertex u, graph& g) 
{
	bool change = false;
	for (unsigned int i = 0; i < g.adj(u).size(); ++i) {
		vertex iv = g.adj(u)[i];
		if (g.info(iv).op == get_row || g.info(iv).op == get_column 
			|| g.info(iv).op == get_row_from_column 
			|| g.info(iv).op == get_column_from_row
			|| g.info(iv).op == get_element) {
			for (unsigned int j = i + 1; j < g.adj(u).size(); ++j) {
				vertex jv = g.adj(u)[j];
				// if these parents equal each other, then are they not already
				// mergerd??  The following check that requires this is confusing
				// This needs more thought.
				if (g.find_parent(jv) == g.find_parent(iv))
					return false;
				if (g.info(jv).op == g.info(iv).op
						&& (g.find_parent(jv) == g.find_parent(iv)
						|| (! (depends_on(g.find_parent(iv), g.find_parent(jv), g)
						|| depends_on(g.find_parent(jv), g.find_parent(iv), g))))) {
					
					// partitioning showed this...
					// g.merge requires subgraphs to be contained within same subgraph
					if (g.find_parent(jv)->parent == g.find_parent(iv)->parent) {

						//std::cerr << "merge gets " << u << std::endl;
	
						g.merge(g.find_parent(jv), g.find_parent(iv));
	
						change = true;
						goto end;
					} //if
				}//if
			}//for
		}//if
	}//for
	end:
	return change;
}

bool check_merge_scalars(vertex u, graph& g)
{
	if (g.info(u).t.k == scalar) {
		bool change = false;
		for (unsigned int i = 0; i != g.adj(u).size(); ++i) {
			vertex iv = g.adj(u)[i];
			if (g.info(iv).t.k == scalar) {
				for (unsigned int j = i + 1; j != g.adj(u).size(); ++j) {
					vertex jv = g.adj(u)[j];
					if (g.info(jv).t.k == scalar
							&& (! (depends_on(g.find_parent(iv), g.find_parent(jv), g)
							|| depends_on(g.find_parent(jv), g.find_parent(iv), g)))) {
						return true;
					}//if
				}//for
			}//if
		}//for
		end:
		return change;
	} 
	return false;
}

bool merge_scalars(vertex u, graph& g)
{
	if (g.info(u).t.k == scalar) {
		bool change = false;
		for (unsigned int i = 0; i != g.adj(u).size(); ++i) {
			vertex iv = g.adj(u)[i];
			if (g.info(iv).t.k == scalar) {
				for (unsigned int j = i + 1; j != g.adj(u).size(); ++j) {
					vertex jv = g.adj(u)[j];
					if (g.info(jv).t.k == scalar
							&& (! (depends_on(g.find_parent(iv), g.find_parent(jv), g)
							|| depends_on(g.find_parent(jv), g.find_parent(iv), g)))) {

						//std::cerr << "merge scalars " << u << std::endl;

						g.merge(g.find_parent(jv), g.find_parent(iv));

						change = true;
						goto end;
					}//if
				}//for
			}//if
		}//for
		end:
		return change;
		} else
	return false;
}

bool check_pipeline(vertex u, graph& g)
{
	// if we have a temporary that is consumed by a get_column
	// and that is produced by a column vector, then 
	// remove the temporary and merge the two subgraphs
	//std::cout << u << "\n";
	int ut = useless_temporary(u,g);
	
	if (ut > -1 && g.mergeable(g.inv_adj(u)[0], g.adj(u)[ut]))
		return true;
	return false;
	
	//return useless_temporary(u,g) && g.mergeable(g.inv_adj(u)[0], g.adj(u)[0]);
}

bool pipeline(vertex u, graph& g)
{
	//std::cout << "inside pipeline " << u << std::endl;

	// if we have a temporary that is consumed by a get_column
	// and that is produced by a column vector, then 
	// remove the temporary and merge the two subgraphs
	int ut = useless_temporary(u,g);
	if (ut > -1	&& g.mergeable(g.inv_adj(u)[0], g.adj(u)[ut])) {

		//std::cerr << "about to pipeline " << u << std::endl;

		vertex pred = g.inv_adj(u)[0];
		vertex succ = g.adj(u)[ut];
		vector<vertex> succ_adj = g.adj(succ);

		for (vector<vertex>::iterator i = succ_adj.begin(); 
				i !=  succ_adj.end(); ++i)
			g.add_edge(pred, *i);

		//std::cerr << "edges added " << u << std::endl;

		subgraph* sg1 = 0, *sg2 = 0;
		sg1 = g.find_parent(pred);
		sg2 = g.find_parent(succ);
		assert(0 != sg1 && 0 != sg2);
		g.merge(sg1, sg2);

		//std::cout << "merged " << u << std::endl;

		if (g.info(u).op != output && g.adj(u).size() == 1) {
			//g.clear_vertex(pred);
			if (g.info(pred).op != sumto)
				g.info(pred).op = temporary;
			g.clear_vertex(u);
		}
		
		// creating this temporary changes the base container of the new temporary
		string bc = g.info(pred).t.get_highest_column()->dim.dim;
		string br = g.info(pred).t.get_highest_row()->dim.dim;
		
		type *t = &g.info(pred).t;
		while (t) {
			t->dim.base_cols = bc;
			t->dim.base_rows = br;
			
			t = t->t;
		}	
		
		g.clear_vertex(succ);
		//std::cerr << "finished clearing " << std::endl;
		return true;
	}
	else
		return false;
}
*/
//////////////////////////////////// END OPTIMIZERS ///////////////////////////////



//////////////////////////////////// OPTIMIZERS NEW ///////////////////////////////

/// given
///      A.0
///   /       \
/// A.1(i)   A.2(i)
/// look from A.0 at A.1 and A.2 to see if they get the same data
/// are unmerged, and their graphs are possitioned for legal fusion

bool in (int id, std::set<int> &s) {
	std::set<int>::iterator i = s.begin();
	for (; i != s.end(); i++)
		if (*i == id)
			return true;
	
	return false;
}

std::set<std::pair<int,int> > check_fuse_loops_new(vertex u, graph& g, std::set<int> &toMerge)
{
	std::set<std::pair<int,int> > compat;
	for (unsigned int i = 0; i < g.adj(u).size(); ++i) {
		vertex iv = g.adj(u)[i];
		
		// we only care to merge if its in our toMerge list.
		int id = g.find_parent(iv) == 0 ? 0 : g.find_parent(iv)->uid;

		if (id == 0 || (! in (id, toMerge)))
			continue;
		
		//std::cout << "check" << id << "\n";
		
		if (g.info(iv).op == get_row || g.info(iv).op == get_column 
			|| g.info(iv).op == get_row_from_column 
			|| g.info(iv).op == get_column_from_row
			|| g.info(iv).op == get_element) {
			for (unsigned int j = i + 1; j < g.adj(u).size(); ++j) {
				vertex jv = g.adj(u)[j];

				// Already fused
				if (g.find_parent(jv) == g.find_parent(iv))
					continue;
			
				// we only care to merge if its in our toMerge list.
				int id1 = g.find_parent(jv) == 0 ? 0 : g.find_parent(jv)->uid;
				if (id1 == 0 || (! in (id1, toMerge)))
					continue;
				
				
				if (g.info(jv).op == g.info(iv).op
					&& (! (depends_on(g.find_parent(iv), g.find_parent(jv), g)
						   || depends_on(g.find_parent(jv), g.find_parent(iv), g)))) {
					// g.merge requires subgraphs to be contained within same subgraph
					if (g.find_parent(jv)->parent == g.find_parent(iv)->parent) {
						compat.insert(std::pair<int,int> (id,id1));
					}
				}//if
			}//for
		}//if
	}//for
	
	return compat;
}

int fuse_loops_new(vertex u, graph& g, std::pair<int,int> &toMerge) 
{
	for (unsigned int i = 0; i < g.adj(u).size(); ++i) {
		vertex iv = g.adj(u)[i];
		
		// we only care to merge if its in our toMerge list.
		int id = g.find_parent(iv) == 0 ? 0 : g.find_parent(iv)->uid;
		
		if (id == 0 || ((id != toMerge.first) && (id != toMerge.second)))
			continue;
		
		if (g.info(iv).op == get_row || g.info(iv).op == get_column 
			|| g.info(iv).op == get_row_from_column 
			|| g.info(iv).op == get_column_from_row
			|| g.info(iv).op == get_element) {
			for (unsigned int j = i + 1; j < g.adj(u).size(); ++j) {
				vertex jv = g.adj(u)[j];
				
				// Already fused
				if (g.find_parent(jv) == g.find_parent(iv))
					continue;
				
				// we only care to merge if its in our toMerge list.
				int id1 = g.find_parent(jv) == 0 ? 0 : g.find_parent(jv)->uid;
				if (id1 == 0 || ((id1 != toMerge.first) && (id1 != toMerge.second)))
					continue;
				
				
				if (g.info(jv).op == g.info(iv).op
					&& (! (depends_on(g.find_parent(iv), g.find_parent(jv), g)
						   || depends_on(g.find_parent(jv), g.find_parent(iv), g)))) {
					// g.merge requires subgraphs to be contained within same subgraph
					if (g.find_parent(jv)->parent == g.find_parent(iv)->parent) {
						//std::cerr << "merge gets " << u << std::endl;
						
						return g.merge(g.find_parent(jv), g.find_parent(iv));
					} //if
				}//if
			}//for
		}//if
	}//for

	return -1;
}

std::set<std::pair<int,int> > check_merge_scalars_new(vertex u, graph& g, std::set<int> &toMerge)
{
	std::set<pair<int,int> > result;
	if (g.info(u).t.k == scalar) {
		for (unsigned int i = 0; i != g.adj(u).size(); ++i) {
			vertex iv = g.adj(u)[i];
			
			// we only care to merge if its in our toMerge list.
			int id = g.find_parent(iv) == 0 ? 0 : g.find_parent(iv)->uid;
			
			if (id == 0 || (! in (id, toMerge)))
				continue;
			
			if (g.info(iv).t.k == scalar) {
				for (unsigned int j = i + 1; j != g.adj(u).size(); ++j) {
					vertex jv = g.adj(u)[j];
					
					// we only care to merge if its in our toMerge list.
					int id1 = g.find_parent(jv) == 0 ? 0 : g.find_parent(jv)->uid;
					if (id1 == 0 || (! in (id1, toMerge)))
						continue;
					
					if (g.info(jv).t.k == scalar
						&& (! (depends_on(g.find_parent(iv), g.find_parent(jv), g)
							   || depends_on(g.find_parent(jv), g.find_parent(iv), g)))) {
						int id0 = g.find_parent(iv) == 0 ? 0 : g.find_parent(iv)->uid;
						int id1 = g.find_parent(jv) == 0 ? 0 : g.find_parent(jv)->uid;
						result.insert(pair<int,int>(id0,id1));
					}//if
				}//for
			}//if
		}//for
	} 

	return result;
}

int merge_scalars_new(vertex u, graph& g, std::pair<int,int> &toMerge)
{
	if (g.info(u).t.k == scalar) {
		for (unsigned int i = 0; i != g.adj(u).size(); ++i) {
			vertex iv = g.adj(u)[i];
			// we only care to merge if its in our toMerge list.
			int id = g.find_parent(iv) == 0 ? 0 : g.find_parent(iv)->uid;
			
			if (id == 0 || ((id != toMerge.first) && (id != toMerge.second)))
				continue;
			
			if (g.info(iv).t.k == scalar) {
				for (unsigned int j = i + 1; j != g.adj(u).size(); ++j) {
					vertex jv = g.adj(u)[j];
					
					// we only care to merge if its in our toMerge list.
					int id1 = g.find_parent(jv) == 0 ? 0 : g.find_parent(jv)->uid;
					if (id1 == 0 || ((id1 != toMerge.first) && (id1 != toMerge.second)))
						continue;
					
					if (g.info(jv).t.k == scalar
						&& (! (depends_on(g.find_parent(iv), g.find_parent(jv), g)
							   || depends_on(g.find_parent(jv), g.find_parent(iv), g)))) {
						
						//std::cerr << "merge scalars " << u << std::endl;
						
						return g.merge(g.find_parent(jv), g.find_parent(iv));
					}//if
				}//for
			}//if
		}//for
	}
	return -1;
}

std::set<std::pair<int,int> > check_pipeline_new(vertex u, graph& g, std::set<int> &toMerge)
{
	// if we have a temporary that is consumed by a get_column
	// and that is produced by a column vector, then 
	// remove the temporary and merge the two subgraphs
	//std::cout << u << "\n";
	
	// NOTE: PROBLEM: useless_temporary can have several adj vertices
	// but it always finds, the same one.  If useless_temporary
	// returns ut, but it is found here to not be valid because of
	// a data dependance, there may exists other adjacent vertices
	// that could be legal pipelines and will be missed.  useless_temporary
	// needs to return a list of vertices that look to make pipelineing legal
	// or this data dependance needs to be pushed into useless_temporary
	
	std::set<std::pair<int,int> > result;
	
	int ut = useless_temporary(u,g);
	if (ut < 0)
		return result;
	
	vertex iv = g.inv_adj(u)[0];
	vertex jv = g.adj(u)[ut];
	int id0 = g.find_parent(iv) == 0 ? 0 : g.find_parent(iv)->uid;
	if (id0 == 0 || (! in(id0,toMerge)))
		return result;
	int id1 = g.find_parent(jv) == 0 ? 0 : g.find_parent(jv)->uid;
	if (id1 == 0 || (! in(id1,toMerge)))
		return result;
	if (g.mergeable(g.inv_adj(u)[0], g.adj(u)[ut])) {
		// must check for other dependencies, this can be seen in gemver
		//	sg0
		// |	\
		// sg1  |
		// |	|
		// sg2	|
		// |	|
		// \   /
		//  sg3
		// sg0 and ag3 appear to be pipable, but sg3 depends on sg0 in two
		// places
		
		// Find this case by finding all vertices that sg0 can get to.  If
		// it can get to a vertice in sg3 that is not the connection between
		// sg0 -> sg3, then there must exists another dependency.
		
		subgraph *sg = g.find_parent(iv);
		for (unsigned int i = 0; i !=sg->vertices.size(); ++i) {
			vertex lu = sg->vertices[i];
			if (check_reachable(lu, g, g.find_parent(jv)->vertices, u, 1, sg)) {
				//std::cout << u << "\t" << jv << "\n";
				return result;
			}
		}
		
		result.insert(pair<int,int>(id0,id1));
	}
		
	return result;
}

int pipeline_new(vertex u, graph& g, std::pair<int,int> &toMerge)
{
	//std::cout << "inside pipeline " << u << std::endl;
	
	// if we have a temporary that is consumed by a get_column
	// and that is produced by a column vector, then 
	// remove the temporary and merge the two subgraphs
	int ut = useless_temporary(u,g);

	if (ut < 0)
		return -1;
	
	vertex pred = g.inv_adj(u)[0];
	vertex succ = g.adj(u)[ut];
	
	int id0 = g.find_parent(pred) == 0 ? 0 : g.find_parent(pred)->uid;
	if (id0 == 0 || ((id0 != toMerge.first) && (id0 != toMerge.second)))
		return -1;
	int id1 = g.find_parent(succ) == 0 ? 0 : g.find_parent(succ)->uid;
	if (id1 == 0 || ((id1 != toMerge.first) && (id1 != toMerge.second)))
		return -1;
	
	if (g.mergeable(pred, succ)) {

		//std::cerr << "about to pipeline " << u << std::endl;
		//std::cout << prnt_detail(&g.info(u).t) << "\n";
		
		vector<vertex> succ_adj = g.adj(succ);
		
		for (vector<vertex>::iterator i = succ_adj.begin(); 
			 i !=  succ_adj.end(); ++i)
			g.add_edge(pred, *i);
		
		//std::cerr << "edges added " << u << std::endl;
		
		subgraph* sg1 = 0, *sg2 = 0;
		sg1 = g.find_parent(pred);
		sg2 = g.find_parent(succ);
		assert(0 != sg1 && 0 != sg2);
		
		string oldLD = g.info(u).t.dim.lead_dim;
		int oldDepth = depth(sg1);
		
		int newID = g.merge(sg1, sg2);
		
		//std::cout << "merged " << u << std::endl;
				
		if (g.info(u).op != output && g.adj(u).size() == 1) {
			//g.clear_vertex(pred);
			if (g.info(pred).op != sumto)
				g.info(pred).op = temporary;
			g.clear_vertex(u);
		}
		
		// creating this temporary changes the base container of the new temporary
		// if this new temporary is moved completely into the the new fused loops
		// then leading dimension will change.  If this temporary is used by another
		// subgraph that requires the complete unfused temporary to exists
		// then leading dimension should not change.
		string bc = g.info(pred).t.get_highest_column()->dim.dim;
		string br = g.info(pred).t.get_highest_row()->dim.dim;
		string ld = "1";
		if (g.info(pred).t.k != scalar) {
			if (g.info(pred).t.get_lowest_ns()->k == row) 
				ld = br;
			else if (g.info(pred).t.get_lowest_ns()->k == column) 
				ld = bc;
			
			// check for lead dimension change
			if (g.adj(u).size() > 1) {
				for (int i = 0; i < g.adj(u).size(); ++i) {
					vertex l = g.adj(u)[i];
					subgraph *p = g.find_parent(l);
					if (depth(p) <= oldDepth) {
						ld = oldLD;
						break;
					}
				}
			}
		}
		
		type *t = &g.info(pred).t;
		while (t) {
			t->dim.base_cols = bc;
			t->dim.base_rows = br;
			t->dim.lead_dim	= ld;
			t = t->t;
		}	
	
		g.clear_vertex(succ);
		//std::cerr << "finished clearing " << succ << std::endl;

		return newID;
	}
	else
		return -1;
}


/////////////////////////////////////////// END OPTIMIZERS NEW ////////////////////////////////


