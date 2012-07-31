#include "syntax.hpp"
#include <deque>
#include "translate_utils.hpp"

using std::deque;

bool been_cleared(vertex u, graph &g, int min_depth) {
	// follow chain of stores up to min_depth looking for sumto nodes
	// a sumto node in this chain means the structure has already been
	// cleared
	
	int start_depth = depth(g.find_parent(u));
	
	while (depth(g.find_parent(u)) > min_depth && depth(g.find_parent(u)) <= start_depth) {
		if (g.adj(u).size() != 1)
			return false;
		u = g.adj(u)[0];
		if (g.info(u).op == sumto)
			return true;
	}
	return false;
}

void init_partitions(vector<subgraph*> &subs, std::ostream &out, vector<string> &def_iters, 
		vector<string> &step) {
	for (int i = 0; i != subs.size(); i++) {
		init_partitions(subs[i]->subs, out, def_iters, step);
		
		string s = subs[i]->step;
		if (s.compare("1") == 0)
			continue;
		if (find(def_iters.begin(), def_iters.end(), s) != def_iters.end())
			continue;
		
		// if the iterator is not 1, then it was introduced as a step
		def_iters.push_back(s);
		
	  	if (s.find("$$") == 0) {
	  		s.erase(0,2);
	  		s = "__s" + s;
	  	}
		
		out << "#ifndef BTO_TILE\n";
		out << "#define " << s << " " << step[depth(subs[i])] << "\n";
		out << "#else\n";
		out << "char *bto" << s << " = getenv(\"BTO" << s << "\");\n";
		out << "const int " << s << " = (const int)strtol(bto" << s << ",NULL,10);\n";
		out << "#endif\n";
		
		//out << "int " << s << " = " << step[depth(subs[i])] << ";\n";
		
	}
}

std::string get_next_elem(type in) {
	if (in.k == scalar)
		return "";
	
	type *t = in.get_lowest_ns();

	if (t == 0)
		std::cout << "ERROR: translate_to_code.cpp: get_next_elem(): unexpected type\n";

	if (in.k == t->k)
		return "";
	else {
		string lead_dim = string(t->dim.lead_dim);
		if (lead_dim.find("$$") == 0) {
			lead_dim.erase(0,2);
	  		lead_dim = "__m" + lead_dim;
	  	}		
		return "*"+lead_dim;
	}
}

std::string get_next_elem_stride(type in) {
	if (in.k == scalar)
		return "";
	
	type *t = in.get_lowest_ns();
	
	if (t == 0)
		std::cout << "ERROR: translate_to_code.cpp: get_next_elem(): unexpected type\n";
	
	if (in.k != t->k)
		return "";
	else {
		string base_cols = string(t->dim.base_cols);
		string base_rows = string(t->dim.base_rows);
		if (base_cols.find("$$") == 0) {
	  		base_cols.erase(0,2);
	  		base_cols = "__m" + base_cols;
	  	}
		if (base_rows.find("$$") == 0) {
	  		base_rows.erase(0,2);
	  		base_rows = "__m" + base_rows;
	  	}
		if (t->k == row)
			return "*" + base_cols;
		else
			return "*" + base_rows;
	}
}

void order_subgraphs(vector<subgraph*> &sgorder, subgraph *sg, graph &g) {
	deque<vertex> order;
  	map<vertex,subgraph*> new_sub;
  	map<vertex,vertex> new_old;
  	
  	if (sg == 0) {
  		vector<vertex> verts;
	  	for (unsigned int i = 0; i != g.num_vertices(); ++i)
	    	if (g.find_parent(i) == 0 && (g.adj(i).size() > 0 || g.inv_adj(i).size() > 0))
	      		verts.push_back(i);
	  	
	  	order_subgraphs(order, new_sub, new_old, 0, verts, g.subgraphs, g);
  	}
  	else {
  		order_subgraphs(order, new_sub, new_old, sg, sg->vertices, sg->subs, g);
  	}
  	
  	map<vertex,subgraph*>::iterator itr = new_sub.begin();
  	for (; itr != new_sub.end(); itr++)
  		sgorder.push_back(itr->second);		  	
}
