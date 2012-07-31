#include "partition.hpp"
#include "build_graph.hpp"
#include "analyze_graph.hpp"
#include "md5wrapper.h"
#include <fstream>

////////////////////////////// PARTITION HELPERS ///////////////////////////////////

void check_depth(int curr, int &max_depth, vector<subgraph*> subs) {
	// count levels of subgraphs
	
	for (int i = 0; i != subs.size(); i++) {
		if (subs[i]->subs.size() == 0) {
			if (curr > max_depth)
				max_depth = curr;
		}
		else {
			check_depth(curr+1, max_depth, subs[i]->subs);
		}
	}
}

void update_graph_dispatch(vertex u, graph &g, vector<bool> &up) {
	//std::cout << u << "\n";
	if (g.info(u).op == trans)
		update_graph(u, g, up);
	
	for (int i = 0; i != g.adj(u).size(); i++) {
		update_graph(g.adj(u)[i], g, up);
	}
	for (int i = 0; i != g.inv_adj(u).size(); i++) {
		update_graph(g.inv_adj(u)[i], g, up);
	}
}

void update_graph(vertex u, graph &g, vector<bool> &up) {
	// propogate partition information
	// this does not handle leading dimension (currently unused)
	//std::cout << "\t" << u << "\t" << g.info(u).t.height << "\n";
		
	switch (g.info(u).op) {
	case input: {
		update_graph_dispatch(u, g, up);
		break;
	}
	case output:
	case temporary: {
		// output and temporary can only have one in edge; if change is above take that change
		if (g.inv_adj(u).size() != 1)
			std::cout << "WARNING: partition.cpp: update_graph(): unexpected type (out, tmp)\n";
		
		vertex v = g.inv_adj(u)[0];
		if (up[v] && up[u])
			break;
		
		if (up[v]) {
			type &old_t = g.info(u).t;
			type *new_t = new type(g.info(v).t);
			old_t = *new_t;
			up[u] = true;
			update_graph_dispatch(u, g, up);
		}
		else if (up[u]) {
			type &old_t = g.info(v).t;
			type *new_t = new type(g.info(u).t);
			old_t = *new_t;
			up[v] = true;
			update_graph_dispatch(v, g, up);
		}
		break;
	}
	case trans: {
		vertex v = g.inv_adj(u)[0];
		if (up[u] && up[v])
			break;

		if (up[v]) {
			type &old_t = g.info(u).t;
			type *new_t = new type(g.info(v).t);
			type *t = new_t;
			// transpose
			while (t) {
				if (t->k == row)
					t->k = column;
				else if (t->k == column)
					t->k = row;
				else
					break;
				t = t->t;
			}
			old_t = *new_t;
			up[u] = true;
			update_graph_dispatch(u, g, up);
		}
		else if (up[u]) {
			type &old_t = g.info(v).t;
			type *new_t = new type(g.info(u).t);
			type *t = new_t;
			// transpose
			while (t) {
				if (t->k == row)
					t->k = column;
				else if (t->k == column)
					t->k = row;
				else
					break;
				t = t->t;
			}
			old_t = *new_t;
			up[v] = true;
			update_graph_dispatch(v, g, up);
		}
		break;
	}
	case add:
	case subtract: {
		vertex l = g.inv_adj(u)[0];
		vertex r = g.inv_adj(u)[1];
		if (up[u] && up[l] && up[r])
			break;
		
		if (up[u]) {
			// result brings change
			type &old_l = g.info(l).t;
			type &old_r = g.info(r).t;
			type *new_t = new type(g.info(u).t);
			old_l = *new_t;
			new_t = new type(g.info(u).t);
			old_r = *new_t;
			up[l] = true;
			up[r] = true;
			update_graph_dispatch(l, g, up);
			update_graph_dispatch(r, g, up);
		}
		else if (up[l]) {
			// left operand brings change
			type &old_u = g.info(u).t;
			type &old_r = g.info(r).t;
			type *new_t = new type(g.info(l).t);
			old_u = *new_t;
			new_t = new type(g.info(l).t);
			old_r = *new_t;
			up[u] = true;
			up[r] = true;
			update_graph_dispatch(u, g, up);
			update_graph_dispatch(r, g, up);
		}
		else if (up[r]) {
			// right operand brings change
			type &old_u = g.info(u).t;
			type &old_l = g.info(l).t;
			type *new_t = new type(g.info(r).t);
			old_u = *new_t;
			new_t = new type(g.info(r).t);
			old_l = *new_t;
			up[u] = true;
			up[l] = true;
			update_graph_dispatch(u, g, up);
			update_graph_dispatch(l, g, up);
		}
		break;
	}
	case multiply: {
		update_mult(u, g, up);
		break;
	}
	default: {
		std::cout << "FINISH: partition.cpp: update_graph(): finish for op "
				  << g.info(u).op << "\n";
		break;
	}
	}
	return;
}

void updateStructure(type *t, string base_rows, string base_cols, string lead_dim) {
	while (t->k != scalar) {
		t->dim.base_rows = base_rows;
		t->dim.base_cols = base_cols;
		t->dim.lead_dim = lead_dim;
		t = t->t;
	}
}

#include "type_analysis.hpp"
void update_mult(vertex u, graph &g, vector<bool> &up) {
	// number of cols of left must equal number of cols of result
	// number of rows of right must equal number of rows of result
	// number of rows of left must equal number of cols of right
	//...except in the case of scaling...
	
	//std::cout << "\t" << u << "\n";
	//std::ofstream out("lower1.dot");
	//print_graph(out, g);
	
	vertex l = g.inv_adj(u)[0];
	vertex r = g.inv_adj(u)[1];

	if (up[u] && up[l] && up[r])
		return;

	// will leave lhr pointing to highest row or a scalar	
	// will leave rhc pointing to highest column or a scalar
	type *lhr = g.info(l).t.get_highest_row();
	type *rhc = g.info(r).t.get_highest_column();

	type *uhr = g.info(u).t.get_highest_row();
	type *uhc = g.info(u).t.get_highest_column();
	
	type *lhc = g.info(l).t.get_highest_column();
	type *rhr = g.info(r).t.get_highest_row();

	int left = 0, right = 0, op_r = 0, op_l = 0, r_op = 0, l_op = 0;
	int op_r_s = 0, r_op_s = 0, op_l_s = 0, l_op_s = 0; 
	int lr = g.info(l).t.num_rows();
	int lc = g.info(l).t.num_cols();
	int rr = g.info(r).t.num_rows();
	int rc = g.info(r).t.num_cols();
	int ur = g.info(u).t.num_rows();
	int uc = g.info(u).t.num_cols();
	
	if (g.info(l).t.k == scalar && g.info(r).t.k == scalar) {
		std::cout << "WARNING: partition.cpp: update_mult(): updating scalar mult?\n";
		return;
	}
	if (g.info(l).t.k != scalar && g.info(r).t.k != scalar) {
		// not scaling
		if (lr != rc) {
			if (up[l])
				right = 1;
			else if (up[r])
				left = 1;
			else
				std::cout << "WARNING: partition.cpp: update_mult(): unexpected l/r\n";
		}
		if (lc != uc) {
			if (up[l])
				op_l = 1;
			else if (up[u])
				l_op = 1;
			else
				std::cout << "WARNING: partition.cpp: update_mult(): unexpected l/u\n";	
		}
		if (rr != ur) {
			if (up[r])
				op_r = 1;
			else if (up[u])
				r_op = 1;
			else
				std::cout << "WARNING: partition.cpp: update_mult(): unexpected r/u\n\t\t"
						  << r << "\t" << u << "\n";
		}
	}
	if (g.info(l).t.k == scalar && g.info(r).t.height != g.info(u).t.height) {
		// scaling right
		if (rr != ur) {
			if (up[r])
				op_r = 1;
			else if (up[u])
				r_op = 1;
			else {
				std::cout << "WARNING: partition.cpp: update_mult(): unexpected r/u\n\t\t"
						  << r << "\t" << u << "\n";
			}
		}
		if (rc != uc) {
			if (up[r])
				op_r_s = 1;
			else if (up[u])
				r_op_s = 1;
			else
				std::cout << "WARNING: partition.cpp: update_mult(): unexpected r/u\n";
		}
	}
	if (g.info(r).t.k == scalar && g.info(l).t.height != g.info(u).t.height) {
		// scaling left
		if (lc != uc) {
			if (up[l])
				op_l = 1;
			else if (up[u])
				l_op = 1;
			else
				std::cout << "WARNING: partition.cpp: update_mult(): unexpected l/u\n";	
		}
		if (lr != ur) {
			if (up[l])
				op_l_s = 1;
			else if (up[u])
				l_op_s = 1;
			else
				std::cout << "WARNING: partition.cpp: update_mult(): unexpected l/u\n";
		}
	}

	if (left + right + op_r + op_l + l_op + r_op + r_op_s + op_r_s + l_op_s + op_l_s == 0)
		return;

	if (left + right + op_r + op_l + l_op + r_op + r_op_s + op_r_s + l_op_s + op_l_s != 1) {
		std::cout << "ERROR: partition.cpp: update_mult(): wrong analysis\n";
		std::cout << left << ";" << right << ";" << op_r << ";" << op_l << ";" << r_op << ";" << l_op << "\n";
		std::cout << up[u] << ";" << up[l] << ";" << up[r] << "\n";
		std::cout << u << "\n";
		std::ofstream out("lower.dot");
		print_graph(out, g);
		exit(0);
	}
	
	if (left) {
		//std::cout << "left\n";
		//std::cout << prnt_detail(&g.info(l).t);
		// left needs updating based on rights information
		type *nw = new type();
		*nw = g.info(r).t;
		nw->k = row;
		nw->height = g.info(l).t.height + 1;
		
		lhr->dim.dim = g.info(r).t.dim.step;
		
		nw->t = new type(g.info(l).t);
		updateStructure(nw, g.info(l).t.dim.base_rows, g.info(l).t.dim.base_cols, 
						g.info(l).t.dim.lead_dim);
		g.info(l).t = *nw;
		//std::cout << prnt_detail(&g.info(l).t);
		up[l] = true;
		update_graph_dispatch(l, g, up);
	}
	else if (right) {
		//std::cout << "right\n";
		//std::cout << prnt_detail(&g.info(r).t);
		// right needs updating based on lefts information
		type *nw = new type();
		*nw = g.info(l).t;
		nw->k = column;
		nw->height = g.info(r).t.height + 1;
		
		rhc->dim.dim = g.info(l).t.dim.step;
		
		nw->t = new type(g.info(r).t);
		updateStructure(nw, g.info(r).t.dim.base_rows, g.info(r).t.dim.base_cols, 
						g.info(r).t.dim.lead_dim);
		g.info(r).t = *nw;
		//std::cout << prnt_detail(&g.info(r).t);
		up[r] = true;
		update_graph_dispatch(r, g, up);
	}
	else if (op_r) {
		//std::cout << "op_r\n";
		//std::cout << prnt_detail(&g.info(u).t);
		// result(u) needs updating based on rights information
		type *nw = new type();
		*nw = g.info(r).t;
		nw->height = g.info(u).t.height + 1;
		
		uhr->dim.dim = g.info(r).t.dim.step;
		
		nw->t = new type(g.info(u).t);
		updateStructure(nw, g.info(u).t.dim.base_rows, g.info(u).t.dim.base_cols, 
						g.info(u).t.dim.lead_dim);
		g.info(u).t = *nw;
		//std::cout << prnt_detail(&g.info(u).t);
		up[u] = true;
		update_graph_dispatch(u, g, up);
	}
	else if (op_l) {
		//std::cout << "op_l\n";
		//std::cout << prnt_detail(&g.info(u).t);
		// result(u) needs updating based on lefts information
		type *nw = new type();
		*nw = g.info(l).t;
		nw->height = g.info(u).t.height + 1;
		
		uhc->dim.dim = g.info(l).t.dim.step;
		
		nw->t = new type(g.info(u).t);
		updateStructure(nw, g.info(u).t.dim.base_rows, g.info(u).t.dim.base_cols, 
						g.info(u).t.dim.lead_dim);
		g.info(u).t = *nw;
		//std::cout << prnt_detail(&g.info(u).t);
		up[u] = true;
		update_graph_dispatch(u, g, up);
	}
	else if (r_op) {
		//std::cout << "r_op\n";
		//std::cout << prnt_detail(&g.info(r).t);
		// right needs updating based on results(u) information
		type *nw = new type();
		*nw = g.info(u).t;
		nw->height = g.info(r).t.height + 1;
		
		rhr->dim.dim = g.info(u).t.dim.step;
		
		nw->t = new type(g.info(r).t);
		updateStructure(nw, g.info(r).t.dim.base_rows, g.info(r).t.dim.base_cols, 
						g.info(r).t.dim.lead_dim);
		g.info(r).t = *nw;
		//std::cout << prnt_detail(&g.info(r).t);
		up[r] = true;
		update_graph_dispatch(r, g, up);
	}
	else if (l_op) {
		//std::cout << "l_op\n";
		//std::cout << prnt_detail(&g.info(l).t);
		// left needs updating based on results(u) information
		type *nw = new type();
		*nw = g.info(u).t;
		nw->height = g.info(l).t.height + 1;
		
		lhc->dim.dim = g.info(u).t.dim.step;
		
		nw->t = new type(g.info(l).t);
		updateStructure(nw, g.info(l).t.dim.base_rows, g.info(l).t.dim.base_cols, 
						g.info(l).t.dim.lead_dim);
		g.info(l).t = *nw;
		//std::cout << prnt_detail(&g.info(l).t);
		up[l] = true;
		update_graph_dispatch(l, g, up);
	}
	else if (op_r_s) {
		//std::cout << "op_r_s\n";
		//std::cout << prnt_detail(&g.info(u).t);
		// scaling and mismatch in columns
		// result(u) needs updating based on rights information
		type *nw = new type();
		*nw = g.info(r).t;
		nw->height = g.info(u).t.height + 1;
		
		uhc->dim.dim = g.info(r).t.dim.step;
		
		nw->t = new type(g.info(u).t);
		updateStructure(nw, g.info(u).t.dim.base_rows, g.info(u).t.dim.base_cols, 
						g.info(u).t.dim.lead_dim);
		g.info(u).t = *nw;
		//std::cout << prnt_detail(&g.info(u).t);
		up[u] = true;
		update_graph_dispatch(u, g, up);
	}
	else if (op_l_s) {
		//std::cout << "op_l_s\n";
		//std::cout << prnt_detail(&g.info(u).t);
		// scaling and mismatch in rows
		// result(u) needs updating based on lefts information
		type *nw = new type();
		*nw = g.info(l).t;
		nw->height = g.info(u).t.height + 1;
		
		uhr->dim.dim = g.info(l).t.dim.step;
		
		nw->t = new type(g.info(u).t);
		updateStructure(nw, g.info(u).t.dim.base_rows, g.info(u).t.dim.base_cols, 
						g.info(u).t.dim.lead_dim);
		g.info(u).t = *nw;
		//std::cout << prnt_detail(&g.info(u).t);
		up[u] = true;
		update_graph_dispatch(u, g, up);
	}
	else if (r_op_s) {
		//std::cout << "r_op_s\n";
		//std::cout << prnt_detail(&g.info(r).t);
		// scaling and mismatch in columns
		// right needs updating based on results(u) information
		type *nw = new type();
		*nw = g.info(u).t;
		nw->height = g.info(r).t.height + 1;
		
		rhc->dim.dim = g.info(u).t.dim.step;
		
		nw->t = new type(g.info(r).t);
		updateStructure(nw, g.info(r).t.dim.base_rows, g.info(r).t.dim.base_cols, 
						g.info(r).t.dim.lead_dim);
		g.info(r).t = *nw;
		//std::cout << prnt_detail(&g.info(r).t);
		up[r] = true;
		update_graph_dispatch(r, g, up);
	}
	else if (l_op_s) {
		//std::cout << "l_op_s\n";
		//std::cout << prnt_detail(&g.info(l).t);
		// scaling and mismatch in rows
		// left needs updating based on results(u) information
		type *nw = new type();
		*nw = g.info(u).t;
		nw->height = g.info(l).t.height + 1;
		
		lhr->dim.dim = g.info(u).t.dim.step;
		
		nw->t = new type(g.info(l).t);
		updateStructure(nw, g.info(l).t.dim.base_rows, g.info(l).t.dim.base_cols, 
						g.info(l).t.dim.lead_dim);
		g.info(l).t = *nw;
		//std::cout << prnt_detail(&g.info(l).t);
		up[l] = true;
		update_graph_dispatch(l, g, up);
	}

	return;
}

bool part_check(type *l, type *r, kind lk, kind rk) {
	// look for the given kind somewhere in the corresponding type
	// return true if both have their requrested kind
	while (l) {
		if (l->k == scalar)
			return false;
		if (l->k == lk)
			break;
		l = l->t;
	}
	while (r) {
		if (r->k == scalar)
			return false;
		if (r->k == rk)
			break;
		r = r->t;
	}
	return true;
}

//////////////////////////////////// END PART HELPERS /////////////////////////////

////////////////////////////// FIND PARTITIONING ///////////////////////////////////
/*
// global for unique partition identifier
int pid = 0;


void find_partitioning(graph& g, std::stack<work_item>& s,
		  vector<rewrite_fun> const &check, vector<rewrite_fun> const &optimizations, vector<algo> const &algos,
		  vector<rewrite_fun> const &rewrites, bool dot, int min_depth)
{
	int max_depth = 8;
	
	graph *pg = new graph(g);
	int stop = 0;
	while (s.size() < 3) {
		for (unsigned int u = 0; u != pg->num_vertices(); u++) {
			for (unsigned int i = 0; i != check.size(); ++i) {
				if (check[i](u, *pg)) {
					//std::cout << "opt " << i << " to " << u << "\n";
					optimizations[i](u, *pg);
					
					//std::ofstream o("lower19.dot");
					//print_graph(o, *pg);
					
					// must update algorithms
					assign_algorithms(algos, *pg);				
					graph *ng = new graph(*pg);
					initial_lower(*ng, algos, rewrites);
					
					//if min_depth not met, continue
					int maxd = 0;
					check_depth(1,maxd, ng->subgraphs);
					if (maxd >= min_depth && maxd <= max_depth) {
						pid++;
						work_item w(ng, -1, -1, "tmpb" + boost::lexical_cast<string>(pid));
						s.push(w);
					}
					if (maxd > max_depth)
						goto end;
					
					//std::cout << i << " done\n";
				}
			}
		}
		stop++;
		if (stop == 20) {
			std::cout << "ERROR: compile.cpp: find_partitioning(): unable to meet partitioning "
					  << "requirements\n";
			std::ofstream out("lower.dot");
		    print_graph(out, *pg); 
			exit(0);
		}
	}
 end:
	if (dot) {
		std::ofstream out("lower2.dot");
		print_graph(out, *pg); 
	}
}
*/

void part_collect_work(vertex u,
		  graph& g, std::stack<work_item>& s,
		  vector<rewrite_fun>const& check,
		  string history,
		  bool & all)
{
    for (unsigned int i = 0; i != check.size(); ++i)
      if (check[i](u, g)) {
		work_item w(new graph(g), i, u, "",1);
		s.push(w);
      }

}

std::string type_to_string(type *t) {
	std::string s;
	while (t) {
		if (t->k == scalar)
			s += "scl";
		else if (t->k == row)
			s += "row";
		else if (t->k == column)
			s += "col";
		t = t->t;
	}
	return s;
}

std::string graph_to_string(graph &g) {
	std::string s;
	for (int i = 0; i != g.num_vertices(); i++) {
		s += type_to_string(&g.info(i).t);
	}
	
	md5wrapper md5;
	return md5.getHashFromString(s);
}

void find_partitioning(graph& upg, std::stack<work_item>& s,
		  vector<rewrite_fun> const &check, vector<rewrite_fun> const &optimizations, vector<algo> const &algos,
		  vector<rewrite_fun> const &rewrites, bool dot, int min_depth, int max_depth)
{
	int pid = 0;
	bool all = false;
	std::stack<work_item> ws;
	vector<string> uid;
	//int cnt = 0;
	
	//ws.push(&upg, -1, -1, 'tmp');
	for (int i = 0; i != upg.num_vertices(); i++) {
		part_collect_work(i,upg,ws,check,"",all);
	}
	
	
	while (!ws.empty()) {
		work_item current = ws.top();
		ws.pop();
		graph& g = *current.g;
		
		if (0 <= current.rewrite_rule) {
			optimizations[current.rewrite_rule](current.u, g);
			
			// must update algorithms
			assign_algorithms(algos, g);				
		}
		
		// use hashing to determine duplicate partitionings
		string id = graph_to_string(g);
		bool unique = true;
		for (int uid_itr = 0; uid_itr != uid.size(); uid_itr++) {
			if (uid[uid_itr].compare(id) == 0) {
				//cnt++;
				unique = false;
				break;
			}
		}
		if (unique)
			uid.push_back(id);
		else {
			current.del();
			continue;
		}
		
		graph *ng = new graph(g);
		initial_lower(*ng, algos, rewrites);
		
		// if min_depth not met, continue
		int maxd = 0;
		check_depth(1,maxd, ng->subgraphs);
		if (maxd >= min_depth && maxd <= max_depth) {
			pid++;
			work_item w(ng, -1, -1, "",1);
			s.push(w);
		}
		else {
			//delete ng;
			ng->del();
		}
		
		if (maxd < max_depth) {
			part_collect_work(current.u,g,ws,check,"",all);
		}
		
		current.del();
		//std::cout << i << " done\n";
	}
	uid.clear();
	//std::cout << "count: " << cnt << "\n";
}

////////////////////////////// END FIND PARTITIONS /////////////////////////////////

//////////////////////////////////// PARTITIONERS /////////////////////////////////

// global counter for unique step identifiers
int suid = 0;

bool partition_add_same_operands(vertex u, graph& g) {
	// partition addition when both operands are the same
	// c = a*a
	if (g.info(u).op == add || g.info(u).op == subtract) {
		if (g.inv_adj(u)[0] != g.inv_adj(u)[1])
			return false;
		
		suid++;
		string step = "$$" + boost::lexical_cast<string>(suid);
		
		type &lt = g.info(g.inv_adj(u)[0]).t;
		type &ut = g.info(u).t;
		
		// create a new type for both operands and result and 
		// new type is same kind as highest level of old type
		// from old<T> to old<recursive_copy(old<T>)> or old<old<T>>
		type *newl = new type(lt);
		type *newu = new type(ut);
		
		lt.t->del();
		ut.t->del();
		
		lt.t = newl;
		ut.t = newu;
		
		lt.height = lt.t->height + 1;
		ut.height = ut.t->height + 1;
		
		lt.dim.step = step;
		ut.dim.step = step;
		
		lt.dim.dim = lt.t->dim.dim;
		lt.t->dim.dim = step;
		ut.dim.dim = ut.t->dim.dim;
		ut.t->dim.dim = step;
		
		vector<bool> *ud = new vector<bool>(g.num_vertices(), false);
		vector<bool> &up = *ud;
		up[u] = true;
		up[g.inv_adj(u)[0]] = true;
		up[g.inv_adj(u)[1]] = true;
		update_graph_dispatch(u, g, up);
		
		// push changes up the graph
		update_graph_dispatch(g.inv_adj(u)[0], g, up);
		update_graph_dispatch(g.inv_adj(u)[1], g, up);
		
		return true;
	}
	return false;
}

bool partition_add(vertex u, graph& g) {
	if (g.info(u).op == add || g.info(u).op == subtract) {
		if (g.inv_adj(u)[0] == g.inv_adj(u)[1])
			return partition_add_same_operands(u,g);
		
		suid++;
		string step = "$$" + boost::lexical_cast<string>(suid);
		
		type &lt = g.info(g.inv_adj(u)[0]).t;
		type &rt = g.info(g.inv_adj(u)[1]).t;
		type &ut = g.info(u).t;
		
		// create a new type for both operands and result and 
		// new type is same kind as highest level of old type
		// from old<T> to old<recursive_copy(old<T>)> or old<old<T>>
		type *newl = new type(lt);
		type *newr = new type(rt);
		type *newu = new type(ut);
		
		lt.t->del();
		rt.t->del();
		ut.t->del();
		
		lt.t = newl;
		rt.t = newr;
		ut.t = newu;
		
		lt.height = lt.t->height + 1;
		rt.height = rt.t->height + 1;
		ut.height = ut.t->height + 1;
			
		lt.dim.step = step;
		rt.dim.step = step;
		ut.dim.step = step;
		
		lt.dim.dim = lt.t->dim.dim;
		lt.t->dim.dim = step;
		rt.dim.dim = rt.t->dim.dim;
		rt.t->dim.dim = step;
		ut.dim.dim = ut.t->dim.dim;
		ut.t->dim.dim = step;
		
		vector<bool> *ud = new vector<bool>(g.num_vertices(), false);
		vector<bool> &up = *ud;
		up[u] = true;
		up[g.inv_adj(u)[0]] = true;
		up[g.inv_adj(u)[1]] = true;
		update_graph_dispatch(u, g, up);
		
		// push changes up the graph
		update_graph_dispatch(g.inv_adj(u)[0], g, up);
		update_graph_dispatch(g.inv_adj(u)[1], g, up);
		
		return true;
	}
	return false;
}

bool check_partition_add(vertex u, graph& g) {
	// in addition both operands and results is partitioned
	if (g.info(u).op == add || g.info(u).op == subtract)
		return true;
	return false;
}

bool part_mult_left_result(vertex u, graph &g) {
	if (g.inv_adj(u).size() != 2)
		return false;
	type &l = g.info(g.inv_adj(u)[0]).t;
	if (g.info(u).op == multiply && part_check(&l, &g.info(u).t, column, column)) {
		
		suid++;
		string step = "$$" + boost::lexical_cast<string>(suid);
				
		type &ut = g.info(u).t;
		
		// find highest column for left(lc) and u(uc)
		type *lc = &l;
		type *uc = &ut;

		lc = lc->get_highest_column();
		uc = uc->get_highest_column();

		if (uc == 0 || lc == 0)
			std::cout << "ERROR: partition.cpp: part_mult_left_result(): unexpected types\n";
		
		// make copy of highest column(newuc, newlc) for new outermost type
		type *newcl = new type();
		type *newuc = new type();
		*newuc = *uc;
		*newcl = *lc;
		
		// new top level gets a size of old top level
		newuc->dim.step = step;
		newcl->dim.step = step;
		
		newuc->dim.dim = uc->dim.dim;
		uc->dim.dim = step;
		newcl->dim.dim = lc->dim.dim;
		lc->dim.dim = step;
		
		type* d = newcl->t;
		newcl->t = new type(l);
		d->del();
		newcl->height = l.height + 1;
		d = newuc->t;
		newuc->t = new type(ut);
		d->del();
		newuc->height = ut.height + 1;

		ut = *newuc;
		l = *newcl;
		
		// push updated type information out to adjacent vertecies
		vector<bool> *ud = new vector<bool>(g.num_vertices(), false);
		vector<bool> &up = *ud;
		up[u] = true;
		up[g.inv_adj(u)[0]] = true;
		//up[g.inv_adj(u)[1]] = true;
		update_graph_dispatch(u, g, up);
		
		// push changes up the graph
		// only the left side changes so only push that
		update_graph_dispatch(g.inv_adj(u)[0], g, up);
		
		//std::ofstream out1("lower1.dot");
	    //print_graph(out1, g);
	    
		return true;
	}
	return false;
}

bool check_part_mult_left_result(vertex u, graph &g) {
	// left operand and result can be partitioned
	if (g.inv_adj(u).size() != 2)
			return false;
		type &l = g.info(g.inv_adj(u)[0]).t;
		if (g.info(u).op == multiply && part_check(&l, &g.info(u).t, column, column))
		return true;
	return false;
}

bool part_mult_right_result(vertex u, graph &g) {
	if (g.inv_adj(u).size() != 2)
		return false;
	type &r = g.info(g.inv_adj(u)[1]).t;
	if (g.info(u).op == multiply && part_check(&g.info(u).t, &r, row, row)) {	

		suid++;
		string step = "$$" + boost::lexical_cast<string>(suid);
				
		type &ut = g.info(u).t;
		// find higherst row for right(rr) and u(ur)
		type *rr = &r;
		type *ur = &ut;
		rr = rr->get_highest_row();
		ur = ur->get_highest_row();

		if (ur == 0 || rr == 0)
			std::cout << "ERROR: partition.cpp: part_mult_right_result(): unexpected types\n";
		
		// make copy of highest row(newrr, newur) for new outermost type
		type *newrr = new type();
		type *newur = new type();
		*newur = *ur;
		*newrr = *rr;	
		
		// new top level gets a size of old top level
		newur->dim.step = step;
		newrr->dim.step = step;
		
		newur->dim.dim = ur->dim.dim;
		ur->dim.dim = step;
		newrr->dim.dim = rr->dim.dim;
		rr->dim.dim = step;
		
		type *d = newrr->t;
		newrr->t = new type(r);
		d->del();
		newrr->height = r.height + 1;
		d = newur->t;
		newur->t = new type(ut);
		d->del();
		newur->height = ut.height + 1;

		ut = *newur;
		r = *newrr;

		// push updated type information out to adjacent vertecies
		vector<bool> *ud = new vector<bool>(g.num_vertices(), false);
		vector<bool> &up = *ud;
		up[u] = true;
		//up[g.inv_adj(u)[0]] = true;
		up[g.inv_adj(u)[1]] = true;
		update_graph_dispatch(u, g, up);
		
		// push changes up the graph
		// only the right side changes so only push that
		update_graph_dispatch(g.inv_adj(u)[1], g, up);
		
		return true;
	}
	return false;
}

bool check_part_mult_right_result(vertex u, graph &g) {
	// right operand and result can be partitions
	if (g.inv_adj(u).size() != 2)
		return false;
	type &r = g.info(g.inv_adj(u)[1]).t;
	if (g.info(u).op == multiply && part_check(&g.info(u).t, &r, row, row))
		return true;
	return false;
}

bool part_mult_scl(vertex u, graph &g) {
	if (g.inv_adj(u).size() != 2)
		return false;
	type &l = g.info(g.inv_adj(u)[0]).t;
	type &r = g.info(g.inv_adj(u)[1]).t;
	if ((l.k == scalar && r.k != scalar) || (l.k != scalar && r.k == scalar)) {			
		suid++;
		string step = "$$" + boost::lexical_cast<string>(suid);
		// find non scalar operand
		vertex ns;
		type *t;
		if (l.k != scalar) {
			t = &l;
			ns = 0;
		}
		else {
			t = &r;
			ns = 1;
		}
	
		type &ut = g.info(u).t;
		
		type *newns = new type();
		type *newu = new type();
		*newns = *t;
		*newu = ut;
		
		// new top level gets a size of old top level
		newu->dim.step = step;
		newns->dim.step = step;
		
		newu->dim.dim = ut.dim.dim;
		ut.dim.dim = step;
		newns->dim.dim = t->dim.dim;
		t->dim.dim = step;
		
		type *d = newns->t;
		newns->t = new type(*t);
		d->del();
		newns->height = t->height + 1;
		d = newu->t;
		newu->t = new type(ut);
		d->del();
		newu->height = ut.height + 1;

		ut = *newu;
		*t = *newns;	
				
		vector<bool> *ud = new vector<bool>(g.num_vertices(), false);
		vector<bool> &up = *ud;
		up[u] = true;
		up[g.inv_adj(u)[0]] = true;
		up[g.inv_adj(u)[1]] = true;
		update_graph_dispatch(u, g, up);
		
		// push changes up the graph
		update_graph_dispatch(g.inv_adj(u)[ns], g, up);
			
		return true;
	}
	return false;
}
bool check_part_mult_scl(vertex u, graph &g) {
	// non scalar and result are partitioned
	if (g.inv_adj(u).size() != 2)
		return false;
	type &l = g.info(g.inv_adj(u)[0]).t;
	type &r = g.info(g.inv_adj(u)[1]).t;
	if ((l.k == scalar && r.k != scalar) || (l.k != scalar && r.k == scalar))
		return true;
	return false;
}

bool check_part_mult_left_right(vertex u, graph &g) {
	// left and right operand can be partitions
	if (g.inv_adj(u).size() != 2)
		return false;
	type &l = g.info(g.inv_adj(u)[0]).t;
	type &r = g.info(g.inv_adj(u)[1]).t;
	if (g.info(u).op == multiply && part_check(&l,&r,row,column))
		return true;
	return false;
}

bool part_mult_left_right(vertex u, graph &g) {
	// left and right operand can be partitions
	if (g.inv_adj(u).size() != 2)
		return false;
	type &l = g.info(g.inv_adj(u)[0]).t;
	type &r = g.info(g.inv_adj(u)[1]).t;
	if (g.info(u).op == multiply && part_check(&l, &r, row, column)) {
		suid++;
		string step = "$$" + boost::lexical_cast<string>(suid);
				
		// find higherst row for left(lr) anr column for right(rc)
		type *lr = &l;
		type *rc = &r;
		lr = lr->get_highest_row();
		rc = rc->get_highest_column();

		if (lr == 0 || rc == 0)
			std::cout << "ERROR: partition.cpp: part_mult_right_result(): unexpected types\n";
		
		// make copy of highest row(newlr, newrc) for new outermost type
		type *newlr = new type();
		type *newrc = new type();
		*newlr = *lr;
		*newrc = *rc;	
		
		// new top level gets a size of old top level
		newlr->dim.step = step;
		newrc->dim.step = step;
		
		newlr->dim.dim = lr->dim.dim;
		lr->dim.dim = step;
		newrc->dim.dim = rc->dim.dim;
		rc->dim.dim = step;
		
		type *d = newrc->t;
		newrc->t = new type(r);
		d->del();
		newrc->height = r.height + 1;
		d = newlr->t;
		newlr->t = new type(l);
		d->del();
		newlr->height = l.height + 1;
		
		l = *newlr;
		r = *newrc;

		// no change to result.. only need to push information up
		vector<bool> *ud = new vector<bool>(g.num_vertices(), false);
		vector<bool> &up = *ud;
		//up[u] = true;
		up[g.inv_adj(u)[0]] = true;
		up[g.inv_adj(u)[1]] = true;

		// push changes up the graph
		update_graph_dispatch(g.inv_adj(u)[0], g, up);
		update_graph_dispatch(g.inv_adj(u)[1], g, up);
		
		return true;
	}
	return false;
}

////////////////////////////////// END PARTITIONERS /////////////////////////////////////////////
