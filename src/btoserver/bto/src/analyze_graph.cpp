#include <fstream>
#include "boost/lexical_cast.hpp"
#include "analyze_graph.hpp"
#include "build_graph.hpp"
#include <set>
#include <string>
#include "type_analysis.hpp"

#define DEBUGGING 0

static int tmp = 0;

using std::vector;

//////////////////////////////// REFINER DEFINITIONS /////////////////////////////////////////////
struct refine_nothing {
	bool operator()(vertex v, graph& g) {
		return false;
	}
};

struct refine_cont_cont {
	refine_cont_cont(vector<algo>* a) :
		algorithms(a) { }
	bool operator()(vertex v, graph& g);
	vector<algo>* algorithms;
};

struct refine_scalar_cont_mult {
	refine_scalar_cont_mult(vector<algo>* a) :
		algorithms(a) { }
	bool operator()(vertex v, graph& g);
	vector<algo>* algorithms;
};

struct refine_cont_cont_stride_mult {
	refine_cont_cont_stride_mult(vector<algo>* a) :
	algorithms(a) { }
	bool operator()(vertex v, graph& g);
	vector<algo>* algorithms;
};

struct refine_cont_cont_mult {
	refine_cont_cont_mult(vector<algo>* a) :
		algorithms(a) { }
	bool operator()(vertex v, graph& g);
	vector<algo>* algorithms;
};

struct refine_dot {
	refine_dot(vector<algo>* a) :
		algorithms(a) { }
	bool operator()(vertex v, graph& g);
	vector<algo>* algorithms;
};

struct refine_outer {
	refine_outer(vector<algo>* a) :
		algorithms(a) { }
	bool operator()(vertex v, graph& g);
	vector<algo>* algorithms;
};

/////////////////////////////////////////// END REFINER DEFS ////////////////////////////////////

/////////////////////////////////////////// RETURN TYPE DEFINITIONS /////////////////////////////

struct ret_nothing {
	type operator()(vertex u, vector<vertex> &v, graph &g) {
		type t(unknown,dim_info());
		return t;
	}
};

struct ret_same {
	type operator()(vertex u, vector<vertex> &v, graph &g) {
		if (v.size() != 2)
			std::cout << "ERROR: analyze_graph.cpp: ret_same struct: unexpected inv_adj size\n";
		if (g.info(v[0]).t.k != g.info(v[1]).t.k) {
			if (g.info(v[0]).t.k == unknown)
				return g.info(v[1]).t;
			else if (g.info(v[1]).t.k == unknown)
				return g.info(v[0]).t;
			else {
				std::cout << "WARNING: analyze_graph.cpp: ret_same struct: unexpected types\n";
				return g.info(v[0]).t;
			}
		}
		// should not matter which side as types should match
		else
			return g.info(v[0]).t;
	}
};

struct ret_trans {
	type operator()(vertex u, vector<vertex> &v, graph &g) {
		if (v.size() != 1)
			std::cout << "ERROR: analyze_graph.cpp: ret_trans struct: unexpected inv_adj size\n";;
		type t(g.info(v[0]).t);
		
		type *r = &t;
		while (r) {
			if (r->k == column)
				r->k = row;
			else if (r->k == row)
				r->k = column;
			r = r->t;
		}
		
		return t;	
	}
};

type *ret_mult_r(type *l, type *r, type *u) {
	// with partitioning and blocking it is possible to know much of the type
	// but if anywhere in the type requires an outer product calculation the
	// type is unknown and must be decided from uses of the result
	if (l->k == row && r->k == row) {
		if (r->t == 0 || u->t == 0) {
			std::cout << "ERROR: analyze_graph.cpp: ret_mult_r(): rr bad types\n";
			exit(0);
		}
		
		if (u->k == row) {
			type *newr = new type();
			*newr = *r;
			
			newr->t = ret_mult_r(l,r->t,u->t);
			newr->height = newr->t->height + 1;

			return newr;
		}
		else if (u->k == column) {
			// if r is row<col<scl>>
			// need col<row<scl>>
			// where size and step of each are swapped.
			// may need to do this recursively with partitioning.
			type *newr = new type(*r);
			newr->k = column;
			newr->t->k = row;
			string tmpS = newr->dim.dim;
			newr->dim.dim = newr->t->dim.dim;
			newr->t->dim.dim = tmpS;
			tmpS = newr->dim.step;
			newr->dim.step = newr->t->dim.step;
			newr->t->dim.step = tmpS;
			
			type *tmp = ret_mult_r(l->t,newr->t,u);
			
			newr->del();
			return tmp;
		}
		else {
			std::cout << "ERROR: analyze_graph.cpp: ret_mult_r(): rr bad types\n";
			exit(0);	
		}
	}
	else if (l->k == column && r->k == column) {
		if (l->t == 0 || u->t == 0) {
			std::cout << "ERROR: analyze_graph.cpp: ret_mult_r(): cc bad types\n";
			exit(0);
		}
		
		if (u->k == column) {
			type *newl = new type();
			*newl = *l;
			
			newl->t = ret_mult_r(l->t,r,u->t);
			newl->height = newl->t->height + 1;
		
			return newl;
		}
		else if (u->k == row) {			
			// if l is col<row<scl>>
			// need row<col<scl>>
			// where size and step of each are swapped.
			// may need to do this recursively with partitioning.
			type *newl = new type(*l);
			newl->k = row;
			newl->t->k = column;
			string tmpS = newl->dim.dim;
			newl->dim.dim = newl->t->dim.dim;
			newl->t->dim.dim = tmpS;
			tmpS = newl->dim.step;
			newl->dim.step = newl->t->dim.step;
			newl->t->dim.step = tmpS;
			
			type *tmp = ret_mult_r(newl->t,r->t,u);
			
			newl->del();
			return tmp;
		}
		else {
			std::cout << "ERROR: analyze_graph.cpp: ret_mult_r(): cc bad types\n";
			exit(0);
		}
		
	}
	else if (l->k == row && r->k == column) {
		if (l->t == 0 || r->t == 0) {
			std::cout << "ERROR: analyze_graph.cpp: ret_mult_r(): rc bad types\n";
			exit(0);
		}
		return ret_mult_r(l->t,r->t,u);
	}
	else if (l->k == column && r->k == row) {
		//u->t here decides the outer product method
		if (u->k == row) {
			type *newr = new type();
			*newr = *r;
			if (r->t == 0 || u->t == 0) {
				std::cout << "ERROR: analyze_graph.cpp: ret_mult_r(): cr(r) bad types\n";
				exit(0);
			}
			newr->t = ret_mult_r(l,r->t,u->t);
			newr->height = newr->t->height + 1;
			return newr;
		}
		else if (u->k == column) {
			type *newl = new type();
			*newl = *l;
			if (l->t == 0 || u->t == 0) {
				std::cout << "ERROR: analyze_graph.cpp: ret_mult_r(): cr(c) bad types\n";
				exit(0);
			}
			newl->t = ret_mult_r(l->t,r,u->t);
			newl->height = newl->t->height + 1;
			return newl;
		}
		else 
			std::cout << "WARNING: analyze_graph(): ret_mult_r(): unexpected types\n";
		return new type();
	}
	else if (l->k == scalar && r->k != scalar) {
		type *newr = new type();
		*newr = *r;
		if (r->t == 0 || u->t == 0) {
			std::cout << "ERROR: analyze_graph.cpp: ret_mult_r(): sc bad types\n";
			exit(0);
		}
		newr->t = ret_mult_r(l,r->t,u->t);
		newr->height = newr->t->height + 1;
		
		return newr;
	}
	else if (l->k != scalar && r->k == scalar) {
		type *newl = new type();
		*newl = *l;
		if (l->t == 0 || u->t == 0) {
			std::cout << "ERROR: analyze_graph.cpp: ret_mult_r(): cs bad types\n";
			exit(0);
		}
		newl->t = ret_mult_r(l->t ,r,u->t);
		newl->height = newl->t->height + 1;
		
		return newl;
	}
	else if (l->k == scalar && r->k == scalar) {
		type *scl = new type();
		*scl = *l;
		return scl;
	}
	else {
		std::cout << "ERROR: analyze_graph.cpp: ret_mult_r(): unexpected types\n";
		return new type();
	}	
}

struct ret_mult {
	type operator() (vertex u, vector<vertex> &v, graph &g) {
		if (v.size() != 2)
			std::cout << "ERROR: analyze_graph.cpp: ret_mult struct: unexpected inv_adj size\n";
		type *l = &(g.info(v[0]).t);
		type *r = &(g.info(v[1]).t);
	
		return *ret_mult_r(l,r,&g.info(u).t);
	}
};

/////////////////////////////////////////// END RETURN TYPE DEFS /////////////////////////////////

////////////////////////////////////////// PARAMETER ACCESS DEFINITIONS //////////////////////////

struct param_nothing {
	vector<param_access> operator() (vector<vertex>& v, vertex u, graph &g) {
		vector<param_access> ret;
		return ret;
	}
};

struct param_two_once {
	vector<param_access> operator() (vector<vertex>& v, vertex u, graph &g) {
		vector<param_access> ret;
		ret.push_back(once);
		ret.push_back(once);
		return ret;
	}
};

struct param_trans {
	vector<param_access> operator() (vector<vertex>& v, vertex u, graph &g) {
		vector<param_access> ret;
		ret.push_back(once);
		return ret;
	}
};

struct param_scale {
	vector<param_access> operator() (vector<vertex>& v, vertex u, graph &g) {
			vector<param_access> ret;
			if (v.size() != 2)
				std::cout << "ERROR: analyze_graph.cpp: param_scale struct: unexpected inv_adj size\n";
			type &l = g.info(v[0]).t;
			type &r = g.info(v[1]).t;
			if (l.k == scalar) {
				ret.push_back(many);
				ret.push_back(once);
			}
			else if (r.k == scalar) {
				ret.push_back(once);
				ret.push_back(many);
			}
			else
				std::cout << "WARNING: analyze_graph.cpp: param_scale struct: unexpected types\n";
			return ret;
		}
};

struct param_cont_cont_stride_mult {
	vector<param_access> operator()(vector<vertex>& v, vertex u, graph &g) {
		vector<param_access> ret;
		// l == r != u
		if (v.size() != 2)
			std::cout << "ERROR: analyze_graph.cpp: param_scale struct: unexpected inv_adj size\n";
		
		if (g.info(u).t.k == row) {
			ret.push_back(once);
			ret.push_back(once);
			
		}
		else if (g.info(u).t.k == column) {
			ret.push_back(once);
			ret.push_back(once);
		}
		
		else
			std::cout << "WARNING: analyze_graph.cpp: param_cont_cont_mult struct: unexpected types\n";
		
		return ret;
	}
};

struct param_cont_cont_mult {
	vector<param_access> operator()(vector<vertex>& v, vertex u, graph &g) {
		vector<param_access> ret;

		if (v.size() != 2)
			std::cout << "ERROR: analyze_graph.cpp: param_scale struct: unexpected inv_adj size\n";
		
		if (g.info(u).t.k == row) {
			ret.push_back(many);
			ret.push_back(once);
			
		}
		else if (g.info(u).t.k == column) {
			ret.push_back(once);
			ret.push_back(many);
		}

		else
			std::cout << "WARNING: analyze_graph.cpp: param_cont_cont_mult struct: unexpected types\n";
		
		return ret;
	}
};

struct param_outer {
	vector<param_access> operator() (vector<vertex>& v, vertex u, graph &g) {
		vector<param_access> ret;
		if (v.size() != 2)
			std::cout << "ERROR: analyze_graph.cpp: param_scale struct: unexpected inv_adj size\n";
		
		//return row<*>
		if (g.info(u).t.k == row) {
			ret.push_back(many);
			ret.push_back(once);
		}
		//return col<*>
		else {
			ret.push_back(once);
			ret.push_back(many);
		}
		
		return ret;
	}
};

struct param_dot {
	vector<param_access> operator() (vector<vertex>& v, vertex u, graph &g) {
		vector<param_access> ret;
		
		if (v.size() != 2)
			std::cout << "ERROR: analyze_graph.cpp: param_scale struct: unexpected inv_adj size\n";
		ret.push_back(once);
		ret.push_back(once);
		
		return ret;
	}
};

////////////////////////////////////////// END PARAM ACCESS DEFS ////////////////////////////////

//////////////////////////////////// ADDITIONAL CONSTRAINTS /////////////////////////////////////

struct constraint_false {
	bool operator() (vertex u, graph &g) {
		return false;
	}
};

struct constraint_true {
	bool operator() (vertex u, graph &g) {
		return true;
	}
};

struct constraint_cont_cont_mult {
	bool operator() (vertex u, graph &g) {
		if (g.inv_adj(u).size() != 2)
			return false;
		
		type *l = &g.info(g.inv_adj(u)[0]).t;
		type *r = &g.info(g.inv_adj(u)[1]).t;
		type *ut = &g.info(g.adj(u)[0]).t;
		
		if (l->k == r->k && l->k == ut->k)
			return true;
		return false;
	}
};

struct constraint_cont_cont_stride_mult {
	bool operator() (vertex u, graph &g) {
		if (g.inv_adj(u).size() != 2)
			return false;
		
		type *l = &g.info(g.inv_adj(u)[0]).t;
		type *r = &g.info(g.inv_adj(u)[1]).t;
		type *ut = &g.info(g.adj(u)[0]).t;
		
		if (l->k == r->k && l->k != ut->k)
			return true;
		return false;
	}
};

//////////////////////////////// END ADDITIONAL CONSTRAINTS /////////////////////////////////////

void init_algos(std::vector<algo>& algos) {
	// Do nothing (0)
	algos.push_back(algo(deleted, ret_nothing(), decouple, param_nothing(), 
						 type(unknown), refine_nothing(),false,constraint_true()));
	
	// Transpose (1-2)
	algos.push_back(algo(trans, ret_trans(), decouple, param_trans(), type(column, dim_info()),
						 refine_nothing(), false,constraint_true()));
	algos.push_back(algo(trans, ret_trans(), decouple, param_trans(), type(row, dim_info()),
						 refine_nothing(), false,constraint_true()));
	
	// Scalar Scalar Multiplication (3)
	algos.push_back(algo(multiply, ret_mult(), decouple, param_two_once(), type(scalar, dim_info()), 
						 type(scalar, dim_info()), refine_nothing(), true,constraint_true()));
	
	// Scalar Vector Multiplication (4-7)
	algos.push_back(algo(multiply, ret_mult(), decouple, param_scale(), type(scalar, dim_info()),
						 type(row, dim_info()), refine_scalar_cont_mult(&algos), 
						 true,constraint_true()));
	algos.push_back(algo(multiply, ret_mult(), decouple, param_scale(), type(row, dim_info()),
						 type(scalar, dim_info()), refine_scalar_cont_mult(&algos), 
						 true,constraint_true()));
	algos.push_back(algo(multiply, ret_mult(), decouple, param_scale(), type(scalar, dim_info()),
						 type(column, dim_info()), refine_scalar_cont_mult(&algos), 
						 true,constraint_true()));
	algos.push_back(algo(multiply, ret_mult(), decouple, param_scale(), type(column, dim_info()),
						 type(scalar, dim_info()), refine_scalar_cont_mult(&algos), 
						 true,constraint_true()));
	
	// Stride 1 Multiply (8-9)
	algos.push_back(algo(multiply, ret_mult(), decouple, param_cont_cont_mult(), 
						 type(row, dim_info()), type(row, dim_info()), refine_cont_cont_mult(&algos), 
						 true,constraint_cont_cont_mult()));
	algos.push_back(algo(multiply, ret_mult(), decouple, param_cont_cont_mult(), 
						 type(column, dim_info()), type(column, dim_info()), 
						 refine_cont_cont_mult(&algos), true,constraint_cont_cont_mult()));
	
	// Stride leading dimension Multiply(10-11)
	algos.push_back(algo(multiply, ret_mult(), decouple, param_cont_cont_stride_mult(), 
						 type(row, dim_info()), type(row, dim_info()), 
						 refine_cont_cont_stride_mult(&algos), true,
						 constraint_cont_cont_stride_mult()));
	algos.push_back(algo(multiply, ret_mult(), decouple, param_cont_cont_stride_mult(), 
						 type(column, dim_info()), type(column, dim_info()), 
						 refine_cont_cont_stride_mult(&algos), true,
						 constraint_cont_cont_stride_mult()));
	
	// Inner Product (12)
	algos.push_back(algo(multiply, ret_mult(), inseparable, param_dot(), type(row, dim_info()), 
						 type(column, dim_info()), refine_dot(&algos), true,constraint_true()));
	
	// Outer Product (13)
	algos.push_back(algo(multiply, ret_mult(), decouple, param_outer(), type(column, dim_info()), 
						 type(row,dim_info()),refine_outer(&algos),true,constraint_true()));
	
	// Scalar Scalar Addition (14)
	algos.push_back(algo(add, ret_same(), decouple, param_two_once(), type(scalar, dim_info()), 
						 type(scalar,dim_info()), refine_nothing(), true,constraint_true()));
	
	// Vector Vector Addition (15-16)
	algos.push_back(algo(add, ret_same(), decouple, param_two_once(), type(column, dim_info()), 
						 type(column, dim_info()), refine_cont_cont(&algos), true,constraint_true()));
	algos.push_back(algo(add, ret_same(), decouple, param_two_once(), type(row, dim_info()), 
						 type(row, dim_info()), refine_cont_cont(&algos), true,constraint_true()));
	
	// Scalar Scalar Subtraction (17)
	algos.push_back(algo(subtract, ret_same(), decouple, param_two_once(), type(scalar, dim_info()), 
						 type(scalar, dim_info()), refine_nothing(), true,constraint_true()));
	
	// Vector Vector Subtraction (18-19)
	algos.push_back(algo(subtract, ret_same(), decouple, param_two_once(), type(column, dim_info()),
						 type(column, dim_info()), refine_cont_cont(&algos), 
						 true,constraint_true()));
	algos.push_back(algo(subtract, ret_same(), decouple, param_two_once(), type(row, dim_info()), 
						 type(row, dim_info()), refine_cont_cont(&algos), true,constraint_true()));
}

void verify_parents_sub(subgraph* g) {
	//std::cerr << g << "(";
	for (unsigned int i = 0; i != g->subs.size(); ++i) {
		assert(g->subs[i]->parent == g);
	}
	//std::cerr << ") ";
}

void verify_parents(graph& g) {
	for (unsigned int i = 0; i != g.num_vertices(); ++i) {
		subgraph* sg = g.find_parent(i);
		if (sg) {
			if (std::find(sg->vertices.begin(), sg->vertices.end(), i) != sg->vertices.end())
				;
			else {
				std::cerr << "subgraph# " << sg << " i: " << i << std::endl;
				for (unsigned int j = 0; j != sg->vertices.size(); ++j)
					std::cerr << sg->vertices[j] << " ";
				std::cerr << std::endl;
				assert(false);
			}
		}
	}

	for (unsigned int i = 0; i != g.subgraphs.size(); ++i) {
		assert(g.subgraphs[i]->parent == 0);
		verify_parents_sub(g.subgraphs[i]);
	}
	//std::cerr << std::endl;
}

void inputs_error(vertex i, subgraph *sg) {
	std::cerr << "\nERROR: " << i << " not included in inputs of subgraph ?";
	//if (sg->start == 0)
	//	std::cerr << "1";
	//else
	//	std::cerr << "?";
	std::cerr << ".." << sg->iterations << " ";
	if (sg->outputs.size() > 0)
		std::cerr << "outputs: ";
	for (int j = 0; j != sg->outputs.size(); j++)
		std::cerr << sg->outputs[j] << " ";
	std::cerr << std::endl << std::endl;

	assert(find(sg->inputs.begin(), sg->inputs.end(), i) != sg->inputs.end());
}

bool check_subgraph(subgraph *current, subgraph *end, vertex v) {
	//std::cout << "\t" << v << "\t" << current << " " << end << std::endl;
	if (current == end)
		return true;
	if (current == 0)
		return false;
	if (check_subgraph(current->parent, end, v)) {
		if (find(current->inputs.begin(), current->inputs.end(), v) == current->inputs.end())
			inputs_error(v, current);
		return true;
	}

	return false;
}

void verify_inputs(graph& g) {
	for (int i = 0; i != g.num_vertices(); i++) {
		subgraph *sg = g.find_parent(i);
		if (sg == 0)
			continue;
		//std::cout << "--: " << i << std::endl;
		std::vector<vertex> vert = g.inv_adj(i);
		for (int j = 0; j != vert.size(); j++) {
			subgraph *p = g.find_parent(vert[j]);
			if (p == sg)
				continue;

			if (check_subgraph(sg->parent, p, i) && find(sg->inputs.begin(), sg->inputs.end(), i)
					== sg->inputs.end()) {
				//inputs not correct
				inputs_error(i, sg);
			}
		}
	}
}

/*
 //perhaps this is not necessary
 bool check_input_rec(subgraph* current, subgraph* end, vertex v) {
 if (current == end)
 return true;
 if (current == 0)
 return false;
 if (check_input_rec(current->parent, end, v)) {
 if (find(current->inputs.begin(), current->inputs.end(), v) == current->inputs.end())
 current->inputs.push_back(v);
 return true;
 }
 
 return false;
 }
 */

void update_inputs(subgraph *sg, graph&g) {
	//sg is a new subgraph.  the inputs vector may be updated from add and remove edge calls
	//correct inputs if any are missed

	/*
	 // vertices that originate here
	 std::vector<vertex>::iterator i = sg->vertices.begin();
	 for (; i != sg->vertices.end(); i++) {
	 vertex v = *i;
	 if (sg->inputs.end() != find(sg->inputs.begin(), sg->inputs.end(), v) ||
	 sg->outputs.end() != find(sg->outputs.begin(), sg->outputs.end(), v))
	 continue;
	 
	 std::vector<vertex> vert = g.inv_adj(v);
	 for (int j = 0; j < vert.size(); j++) {
	 subgraph *p = g.find_parent(vert[j]);
	 if (p == sg)
	 continue;
	 if (check_input_rec(sg->parent,p,v))
	 if (find(sg->inputs.begin(), sg->inputs.end(), v) == sg->inputs.end())
	 sg->inputs.push_back(v);
	 }
	 }
	 */

	// vertices that only pass through (dependants)
	std::vector<subgraph*>::iterator k = sg->subs.begin();
	std::set<vertex> ins;
	for (; k != sg->subs.end(); k++) {
		ins.insert((*k)->inputs.begin(), (*k)->inputs.end());
	}

	std::set<vertex>::iterator itr = ins.begin();
	for (; itr != ins.end(); itr++) {
		if (find(sg->vertices.begin(), sg->vertices.end(), *itr) == sg->vertices.end())
			sg->inputs.push_back(*itr);
	}
}

bool is_summation(vertex u, graph& g) {
	subgraph* p = g.find_parent(u);
	return p && std::find(p->summations.begin(), p->summations.end(), u) != p->summations.end();
}

bool types_match(type t1, type t2) {
	return (t1 == t2) || (t1 == unknown) || (t2 == unknown);
}
bool total_type_match(type &t1, type &t2) {
	if (t1.height != t2.height)
		return false;
	type *l = &t1;
	type *r = &t2;
	while (l && r) {
		if (l->k != r->k)
			return false;
		l = l->t;
		r = r->t;
	}
	return true;
}

std::vector<unsigned int> find_matches(vertex u, graph& g, vector<algo> const& algos) {
	std::vector<unsigned int> ret;
	op_code op = g.info(u).op;
	//std::cout << u << std::endl;
	for (unsigned int i = 0; i != algos.size(); ++i) {
		if (algos[i].op == op && algos[i].param_types.size() == g.inv_adj(u).size()) {
			//std::cout << "\t" << op << " : " << i << std::endl;
			//std::cout << "\t" << g.info(g.inv_adj(u)[0]).t.k << std::endl;
			//std::cout << "\t" << g.info(g.inv_adj(u)[1]).t.k << std::endl;
			bool all_match = true;
			for (unsigned int j = 0; j != g.inv_adj(u).size(); ++j) {
				all_match = all_match && types_match(algos[i].param_types[j], g.info(g.inv_adj(u)[j]).t);
			}
			if (all_match) {
				//std::cout << "\t\t" << i << "\n";
				//std::cout << u << "\n";
				type algo_ret_t(algos[i].return_type(u, g.inv_adj(u),g));
				//std::cout << "\t\t" << algo_ret_t.height << "\t" << algo_ret_t.k << "\n";
				all_match = total_type_match(algo_ret_t, g.info(u).t);
				
				if (g.info(u).op == multiply) {
					if (algo_ret_t.t)
						algo_ret_t.t->del();
				}
				
				all_match = all_match && algos[i].additionalConstraints(u, g);
				
				if (all_match == true) {
					//std::cout << "\t\t" << i << "\n";
					ret.push_back(i);
				}
			}
		}
	}
	
	return ret;
}

void apply_match(unsigned int i, vertex u, graph& g, vector<algo> const& algos) {
	g.info(u).algo = i;
	for (unsigned int j = 0; j != g.inv_adj(u).size(); ++j) {
		if (types_match(g.info(g.inv_adj(u)[j]).t, algos[i].param_types[j])) {
			vertex_info& info = g.info(g.inv_adj(u)[j]);
			if (info.t.k == unknown)
				info.t = algos[i].param_types[j];
		}
		else {
			std::cerr << "bad type update: " << g.info(u).to_string(u) << " " 
				<< type_to_string(algos[i].param_types[j]) << std::endl;
			exit(-1);
		}
	}
}

void assign_algorithms(vector<algo> const& algos, graph& g) {
	int ambigs = 0;
	int prev_ambigs;
	std::vector<unsigned int> matches;
	do {
		prev_ambigs = ambigs;
		ambigs = 0;
		for (unsigned int u = 0; u != g.num_vertices(); ++u) {
			matches = find_matches(u, g, algos);
			if (matches.size() == 1) {
				//std::cout << u << "\t" << matches[0] << std::endl;
				apply_match(matches[0], u, g, algos);
				//std::ofstream out("lower.dot");
				//print_graph(out, g); 
			}
			else if (matches.size() > 1) {
				
				std::cout << u << ": ";
				std::vector<unsigned int>::iterator itr;
				for (itr = matches.begin(); itr != matches.end(); itr++) {
					std::cout << *itr << ", ";
				}
				std::cout << std::endl;
				//std::ofstream out("lower.dot");
				//print_graph(out, g);
				
				++ambigs;
			}
			//else { 
			//std::cout << "--" << u << std::endl;
			//std::ofstream out("lower.dot");
			//print_graph(out, g);
			//}
		}
		//std::cout << ambigs << " ambiguous" << std::endl;
	} while (ambigs != prev_ambigs);

	if (ambigs > 0) {
		std::cerr << "ambiguous " << ambigs << std::endl;
		for (int i = 0; i < matches.size(); i++)
			std::cout << "match: " << matches[i] << std::endl;
		exit(-1);
	}
}

void assign_work_r(vertex u, vector<bool>& marked, vector<algo> const& algos, graph& g) {
	bool triv = true;
	marked[u] = true;

	for (graph::inv_adj_iter vi = g.inv_adj(u).begin(); vi != g.inv_adj(u).end(); ++vi) {
		if (!marked[*vi])
			assign_work_r(*vi, marked, algos, g);
		triv = triv && g.info(*vi).trivial;

		switch (g.info(u).op) {
		case input:
		case output:
		case temporary:
			break;
		default:
			//std::cout << g.info(u).op << std::endl;
			int i = g.info(u).algo;
			//std::cout << "\t" << u << std::endl;
			assert(i < algos.size());
			vector<param_access> p_acc;
			p_acc = algos[i].param_accesses(g.inv_adj(u), u, g);

			for (unsigned int j = 0; j != g.inv_adj(u).size(); ++j) {
				vertex_info& info = g.info(g.inv_adj(u)[j]);
				if (p_acc[j] == many && !info.trivial) {
					info.eval = evaluate;
					info.trivial = true;
					info.label = "tmp" + boost::lexical_cast<string>(tmp++);
				}
			}
		}
	}
	bool does_work;
	switch (g.info(u).op) {
		case input: case output: case temporary:
		does_work = false;
		break;
		default:
		does_work = algos[g.info(u).algo].does_work;

		int i = g.info(u).algo;
		if (algos[i].comp == inseparable) {
			g.info(u).eval = evaluate;
			g.info(u).trivial = true;
			g.info(u).label = "tmp" + boost::lexical_cast<string>(tmp++);
		}
	}
	g.info(u).trivial = (triv && !does_work) || g.info(u).eval == evaluate;
}

void assign_work(vector<algo> const& algos, graph& g) {
	vector<bool> marked(g.num_vertices(), false);
	for (unsigned int u = 0; u != g.num_vertices(); ++u) {
		if (!marked[u])
			assign_work_r(u, marked, algos, g);
	}
}

void reintroduce_variables(graph& g) {
	for (vertex u = 0; u != g.num_vertices(); ++u) {
		//std::cout << "processing " << u << std::endl;
		if (g.info(u).eval == evaluate) {
			vertex_info vi = g.info(u);
			vi.op = temporary;
			vi.eval = defer;
			vertex nu = g.add_vertex(vi);
			std::vector<vertex> adj(g.adj(u));
			for (unsigned int vi = 0; vi != adj.size(); ++vi) {
				g.remove_edges(u, adj[vi]);
				g.add_edge(nu, adj[vi]);
			}
		}
	}
}

void initial_lower(graph &g, vector<algo> const &algos, vector<rewrite_fun> const &rewrites) {
  for (int i = 5; i != 30; ++i) {
    //std::cerr << "lowering" << std::endl;

//#define DB
#ifdef DB
    {
    	std::ofstream out(string("lower" + boost::lexical_cast<string>(i) 
    		+ ".dot").c_str());
    	print_graph(out, g);
    }
#endif
    lower_graph(algos, g);
    verify_parents(g);
    apply_rewrites(rewrites, g);
#ifdef DB
    {
    	std::ofstream out(string("rw" + boost::lexical_cast<string>(i) 
	       + ".dot").c_str());
  		print_graph(out, g);
    }
#endif
  }
}

void lower_graph(vector<algo> const& algos, graph& g) {
	unsigned int n = g.num_vertices();
	for (unsigned int u = 0; u != n; ++u) {
		switch (g.info(u).op) {
		case trans:
		case negate_op:
		case add:
		case subtract:
		case multiply: {
			//std::cout << "lowering " << u << " " << g.info(u).label << std::endl;
			unsigned int i = g.info(u).algo;
			//std::cout << "\talgo " << i << "\tvertex " << u << std::endl;
			//std::ofstream out("lower.dot");
			//print_graph(out, g);
			assert(i < algos.size());
			algos[i].refiner(u, g);
			break;
		}
		default:
			break;
		}
	}
}

void apply_rewrites(vector<rewrite_fun>const& rewrites, graph& g) {
	bool change = true;
	do {
		change = false;
		for (vertex u = 0; u != g.num_vertices(); ++u) {
			for (unsigned int i = 0; i != rewrites.size(); ++i) {
				change = rewrites[i](u, g);
				if (change) {
					//std::cout << "\talgo " << i << "\tvertex " << u << std::endl;
					verify_parents(g);
					//verify_inputs(g);
					break;
				}
			}//for rewrites
		}// for vertices
	} while (change);
}

op_code get_op(type t, bool reverse) {
	if (t.k == scalar) {
		return get_element;
	} else if (t.k == row) {
		if (reverse)
			return get_row_from_column;
		else
			return get_row;
	} else if (t.k == column) {
		if (reverse)
			return get_column_from_row;
		else
			return get_column;
	} else
		std::cout << "ERROR: analyze_graph.cpp: store_op(): unknown kind\n";
	exit(-1);
}

op_code store_op(type t, bool storeAdd) {
	if (t.k == scalar) {
		if (storeAdd)
			return store_add_element;
		else
			return store_element;
	} else if (t.k == row) {
		if (storeAdd)
			return store_add_row;
		else
			return store_row;
	} else if (t.k == column) {
		if (storeAdd)
			return store_add_column;
		else
			return store_column;
	} else
		std::cout << "ERROR: analyze_graph.cpp: store_op(): unknown kind\n";
	exit(-1);
}

////////////////////////////////// REFINERS ////////////////////////////////////////////////////

string getLoopStep(type *t) {
	// determine how a loop should step based on the given type
	
	kind k = t->k;
	string step = t->dim.step;
	
	t = t->t;
	while (t) {
		if (t->k == k)
			return step;
		t = t->t;
	}
	return "1";
}

bool refine_cont_cont::operator()(vertex v, graph& g) {

	int left = g.inv_adj(v)[0];
	int right = g.inv_adj(v)[1];

	if (g.info(left).t.k != g.info(right).t.k)
		std::cout << "WARNING: analyze_graph.cpp: refine_cont_cont(): type mismatch\n";

	type tl = *(g.info(left).t.t);
	type tr = *(g.info(right).t.t);
	op_code op, op2;
	op = get_op(tl, false);
	op2 = store_op(*g.info(v).t.t, false);

	vertex_info getl(tl, op, g.info(left).label);
	int getl_v = g.add_vertex(getl);
	op = get_op(tr, false);
	vertex_info getr(tr, op, g.info(right).label);
	int getr_v = g.add_vertex(getr);
	vertex_info newop(*g.info(v).t.t, g.info(v).op, "");
	int newop_v = g.add_vertex(newop);
	vertex_info store(*g.info(v).t.t, op2, g.info(v).label);
	int store_v = g.add_vertex(store);

	subgraph* sg = new subgraph("iterate over " + g.info(v).label,
			g.find_parent(v), g, g.info(v).t.dim.dim, getLoopStep(&g.info(v).t));

	sg->vertices.push_back(getl_v);
	sg->vertices.push_back(getr_v);
	sg->vertices.push_back(newop_v);
	sg->vertices.push_back(store_v);

	g.remove_edges(left, v);
	g.remove_edges(right, v);
	
	g.add_edge(left, getl_v);
	g.add_edge(getl_v, newop_v);
	g.add_edge(right, getr_v);
	g.add_edge(getr_v, newop_v);
	g.add_edge(newop_v, store_v);
	g.add_edge(store_v, v);

	g.info(v).op = temporary;

	std::vector<unsigned int> matches = find_matches(newop_v, g, *algorithms);
	//std::ofstream out("lower96.dot");
	//print_graph(out, g);
	assert(matches.size() == 1);
	apply_match(matches[0], newop_v, g, *algorithms);

	return true;
}

bool refine_scalar_cont_mult::operator()(vertex v, graph& g) {
	int oldleft = g.inv_adj(v)[0];
	int oldright = g.inv_adj(v)[1];

	vertex n = g.adj(v)[0];
	bool storeAdd = false;
	if (g.info(n).op == sumto || g.info(n).op == store_add_row
		|| g.info(n).op == store_add_column)
		storeAdd = true;
	
	int cont, sclr;
	if (g.info(oldleft).t == scalar) {
		sclr = oldleft;
		cont = oldright;
	} else {
		sclr = oldright;
		cont = oldleft;
	}
	
	if (!(g.info(cont).t.t))
		std::cout << "analyze_graph.cpp: refine_scalar_cont(): unexpected types\n";
	
	type t(*(g.info(cont).t.t));
	op_code op, op2;
	op = get_op(t, false);
	op2 = store_op(*g.info(v).t.t, storeAdd);
	
	if (is_summation(v,g) && op2 == store_element) {
		op2 = store_add_element;
	}

	vertex_info get(t, op, g.info(cont).label);
	int get_v = g.add_vertex(get);
	vertex_info newop(*g.info(v).t.t, g.info(v).op, "");
	int newop_v = g.add_vertex(newop);
	vertex_info store(*g.info(v).t.t, op2, g.info(v).label);
	int store_v = g.add_vertex(store);

	subgraph* sg = new subgraph("iterate over " + g.info(v).label,
			g.find_parent(v), g, g.info(v).t.dim.dim, 
			getLoopStep(&g.info(v).t));

	sg->vertices.push_back(get_v);
	sg->vertices.push_back(newop_v);
	sg->vertices.push_back(store_v);

	g.remove_edges(cont, v);
	g.remove_edges(sclr, v);
	
	g.add_edge(cont, get_v);
	g.add_edge(newop_v, store_v);
	g.add_edge(store_v, v);
	//preserve left and right operands
	if (oldleft == cont) {
		g.add_edge(get_v, newop_v);
		g.add_edge(sclr, newop_v);
	}
	else {
		g.add_edge(sclr, newop_v);
		g.add_edge(get_v, newop_v);
	}
	
	g.info(v).op = temporary;

	std::vector<unsigned int> matches = find_matches(newop_v, g, *algorithms);
	//std::ofstream out("lower.dot");
	//print_graph(out, g);
	assert(matches.size() == 1);
	apply_match(matches[0], newop_v, g, *algorithms);

	return true;
}

bool refine_cont_cont_stride_mult::operator()(vertex v, graph& g) {
	vertex left = g.inv_adj(v)[0];
	vertex right = g.inv_adj(v)[1];
	
	if (g.info(left).t.k == g.info(right).t.k 
		&& g.info(left).t.k == g.info(v).t.k) {
		// unit stride
		return false;
	}
	
	if (g.info(left).t.height != 2 || g.info(right).t.height != 2
		|| g.info(v).t.height != 2) {
		std::cout << "WARNING: analyze_graph.cpp; refine_cont_cont_stride_mult(); "
				 << "unexpected types\n" << g.info(left).t.height << "\n";
	}
		
	vertex stride1, strideld;
	
	if (g.info(left).t.k == column) {
		stride1 = right;
		strideld = left;
	}
	else {
		stride1 = left;
		strideld = right;
	}

	type ts1 = *(g.info(stride1).t.t);
	type tsld = *(g.info(strideld).t.t);

	tsld.dim.dim = g.info(strideld).t.dim.dim;
	tsld.dim.step = g.info(strideld).t.dim.step;
	if (tsld.k == row) 
		tsld.k = column;
	else 
		tsld.k = row;
	
	op_code op;
	
	op = get_op(ts1, false);
	vertex_info gets1(ts1, op, g.info(stride1).label);
	int gets1_v = g.add_vertex(gets1);
	
	op = get_op(tsld, true);
	vertex_info getsld(tsld, op, g.info(strideld).label);
	int getsld_v = g.add_vertex(getsld);

	vertex_info newop(g.info(v).t,g.info(v).op,"");
	int newop_v = g.add_vertex(newop);
	
	subgraph* sg = new subgraph("iterate over " + g.info(stride1).label 
					+ " and " + g.info(strideld).label,g.find_parent(v),
								g, g.info(stride1).t.dim.dim,
								getLoopStep(&g.info(stride1).t));
	sg->summations.push_back(newop_v);
	
	sg->vertices.push_back(gets1_v);
	sg->vertices.push_back(getsld_v);
	sg->vertices.push_back(newop_v);
	
	g.remove_edges(left, v);
	g.remove_edges(right, v);
	// preserve left and right operands
	if (stride1 == left) {
		g.add_edge(left, gets1_v);
		g.add_edge(right, getsld_v);
		g.add_edge(gets1_v, newop_v);
		g.add_edge(getsld_v, newop_v);
	}
	else {
		g.add_edge(left, getsld_v);
		g.add_edge(right, gets1_v);
		g.add_edge(getsld_v, newop_v);
		g.add_edge(gets1_v, newop_v);
	}
	g.add_edge(newop_v, v);
	
	g.info(v).op = sumto;
	
	std::vector<unsigned int> matches = find_matches(newop_v, g, *algorithms);
	//std::ofstream out("lower99.dot");
	//print_graph(out, g);
	assert(matches.size() == 1);
	apply_match(matches[0], newop_v, g, *algorithms);
	
	return true;
}

bool refine_cont_cont_mult::operator()(vertex v, graph& g) {
	vertex left = g.inv_adj(v)[0];
	vertex right = g.inv_adj(v)[1];

	if (g.info(left).t.k == g.info(right).t.k 
		&& g.info(left).t.k != g.info(v).t.k) {
		// strided
		return false;
	}
	
	vertex n = g.adj(v)[0];
	bool storeAdd = false;
	if (g.info(n).op == sumto || g.info(n).op == store_add_row
		|| g.info(n).op == store_add_column)
		storeAdd = true;
	
	vertex matrix;
	vertex vector;
	if (g.info(left).t.k == row) {
		matrix = right;
		vector = left;
	}
	else {
		matrix = left;
		vector = right;
	}

	type tm = *(g.info(matrix).t.t);
	op_code op;
	op = get_op(tm, false);

	vertex_info getm(tm, op, g.info(matrix).label);
	int getm_v = g.add_vertex(getm);
	vertex_info newop(*g.info(v).t.t, g.info(v).op,"");
	int newop_v = g.add_vertex(newop);
	op = store_op(*g.info(v).t.t, storeAdd);
	vertex_info store(*g.info(v).t.t, op, g.info(v).label);
	int store_v = g.add_vertex(store);
	
	subgraph* sg = new subgraph("iterate over " + g.info(matrix).label,
			g.find_parent(v), g, g.info(matrix).t.dim.dim,
			getLoopStep(&g.info(matrix).t));

	sg->vertices.push_back(getm_v);
	sg->vertices.push_back(newop_v);
	sg->vertices.push_back(store_v);

	g.remove_edges(matrix, v);
	g.remove_edges(vector, v);
	g.add_edge(matrix, getm_v);
	// preserve left and right operands
	if (matrix == left) {
		g.add_edge(getm_v, newop_v);
		g.add_edge(vector, newop_v);
	}
	else {
		g.add_edge(vector, newop_v);
		g.add_edge(getm_v, newop_v);
	}
	g.add_edge(newop_v, store_v);
	g.add_edge(store_v, v);

	g.info(v).op = temporary;

	std::vector<unsigned int> matches = find_matches(newop_v, g, *algorithms);
	//std::ofstream out("lower98.dot");
	//print_graph(out, g);
	assert(matches.size() == 1);
	apply_match(matches[0], newop_v, g, *algorithms);

	return true;
}

bool refine_outer::operator()(vertex v, graph& g) {
	int left = g.inv_adj(v)[0];
	int right = g.inv_adj(v)[1];

	vertex n = g.adj(v)[0];
	bool storeAdd = false;
	if (g.info(n).op == sumto || g.info(n).op == store_add_row
			|| g.info(n).op == store_add_column)
		storeAdd = true;

	vertex many, once;
	if (g.info(v).t.k == row) {
		//result is row<col<*>>
		many = left;
		once = right;
	}
	else {
		//result is col<row<*>>
		many = right;
		once = left;
	}

	type to = *(g.info(once).t.t);
	op_code op, op2;
	op = get_op(to, false);
	op2 = store_op(*g.info(v).t.t, storeAdd);
	
	subgraph* sg = new subgraph("iterate over rows of " + g.info(once).label,
								g.find_parent(v), g, g.info(once).t.dim.dim, 
								getLoopStep(&g.info(once).t));
	 
	vertex_info geto(to, op, g.info(once).label);
	int geto_v = g.add_vertex(geto);
	vertex_info newop(*g.info(v).t.t, g.info(v).op, "");
	int newop_v = g.add_vertex(newop);
	vertex_info store(*g.info(v).t.t, op2, g.info(v).label);
	int store_v = g.add_vertex(store);
	
	sg->vertices.push_back(geto_v);
	sg->vertices.push_back(newop_v);
	sg->vertices.push_back(store_v);
	
	g.remove_edges(many, v);
	g.remove_edges(once, v);
	g.add_edge(once, geto_v);
	g.add_edge(newop_v, store_v);
	g.add_edge(store_v, v);
	
	//preserve left and right operand
	if (many == left) {
		g.add_edge(many, newop_v);
		g.add_edge(geto_v, newop_v);
	}
	else {
		g.add_edge(geto_v, newop_v);
		g.add_edge(many, newop_v);
	}
	
	g.info(v).op = temporary;

	std::vector<unsigned int> matches = find_matches(newop_v, g, *algorithms);
	//std::ofstream out("lower98.dot");
	//print_graph(out, g);
	assert(matches.size() == 1);
	apply_match(matches[0], newop_v, g, *algorithms);

	return true;
}

bool refine_dot::operator()(vertex v, graph& g)
{
	int left = g.inv_adj(v)[0];
	int right = g.inv_adj(v)[1];
	
	type tl = *(g.info(left).t.t);
	type tr = *(g.info(right).t.t);
	op_code op, op2;
	op = get_op(tl, false);
	op2 = store_op(tl, false);

	vertex_info getl(tl, op, g.info(left).label);
	int getl_v = g.add_vertex(getl);
	op = get_op(tr, false);
	vertex_info getr(tr, op, g.info(right).label);
	int getr_v = g.add_vertex(getr);
	vertex_info newop(g.info(v).t, g.info(v).op, "");
	int newop_v = g.add_vertex(newop);

	subgraph* sg = new subgraph("", g.find_parent(v), g, g.info(left).t.dim.dim,
			getLoopStep(&g.info(left).t));
	sg->summations.push_back(newop_v);

	sg->vertices.push_back(getl_v);
	sg->vertices.push_back(getr_v);
	sg->vertices.push_back(newop_v);

	g.remove_edges(left, v);
	g.remove_edges(right, v);
	g.add_edge(left, getl_v);
	g.add_edge(right, getr_v);
	g.add_edge(getl_v, newop_v);
	g.add_edge(getr_v, newop_v);
	g.add_edge(newop_v, v);
	
	g.info(v).op = sumto;

	std::vector<unsigned int> matches = find_matches(newop_v, g, *algorithms);
	//std::ofstream out("lower.dot");
	//print_graph(out, g);
	assert(matches.size() == 1);
	apply_match(matches[0], newop_v, g, *algorithms);

	return true;
}

/////////////////////////////////////// END REFINERS ///////////////////////////////////////////

/////////////////////////////////////// REWRITERS - ALWAYS PERFORM /////////////////////////////

bool flip_transpose(vertex u, graph& g) {

	if (g.info(u).op == trans && g.adj(u).size() >= 1
		&& (g.info(u).t.k == row || g.info(u).t.k == column)) {
		
		op_code adjOp = g.info(g.adj(u)[0]).op;
		if (adjOp != get_row && adjOp != get_column 
			&& adjOp != get_element)  {
				return false;
		}

		for (int i = 1; i < g.adj(u).size(); ++i) {
			if (adjOp != g.info(g.adj(u)[i]).op)
				return false;			
		}

		//std::ofstream out1("lower96.dot");
		//print_graph(out1, g);
		
		//std::cerr << "flipping transpose " << u << std::endl;

		vertex pred = g.inv_adj(u)[0];
		type *newu = new type(*(g.info(pred).t.t));
		vertex_info tmp = g.info(u);
		tmp.t = g.info(u).t;	// need to fire the type
								// assigment operator to 
								// get this copied correctly
		
		g.info(u) = g.info(g.adj(u)[0]);
		g.info(u).op = get_op(*newu, false);
		g.info(u).t = *newu;
		
		subgraph* up = g.find_parent(u);

		const vector<vertex> &adjU = g.adj(u);
		for (int i = 0; i < adjU.size(); ++i) {
			vertex succ = adjU[i];
			
			// stride 1 flip
			type *news = new type(*newu);
		
			g.info(succ) = tmp;
			g.info(succ).t = *news;
		
			news = &g.info(succ).t;
			while (news) {
				if (news->k == column)
					news->k = row;
				else if (news->k == row)
					news->k = column;
				news = news->t;
			}
		
			// if there are multuple adj vertices to u
			// must make new vertices of same type as the
			// new u and place them in the correct subgraph
			//     pred             pred
			//      |              /    \
			//  trans (u)		newU    copy(newU)
			//   /    \    to    |			|
			//  1      2		new1	   new2
			//
			// if this does not happen, the transpose will
			// take place in only one of the subgraphs.
			vertex toAdd;
			if (i) {
				vertex_info copyNewU = g.info(u);
				toAdd = g.add_vertex(copyNewU);
				g.add_edge(pred, toAdd);
				g.add_edge(toAdd, succ);
				g.remove_edges(u, succ);
			}
			else {
				toAdd = u;
			}
			
			subgraph* sp = g.find_parent(succ);
		
			if (sp)
				sp->vertices.push_back(toAdd);
		}
		
		if (up)
			remove(up->vertices, u);
		
		tmp.del();
		
		//std::ofstream out("lower97.dot");
		//print_graph(out, g);
		
		return true;
	}
	return false;
}

bool flip_transpose_stride(vertex u, graph& g) {
	if (g.info(u).op == trans && g.adj(u).size() == 1 
		&& (((g.info(u).t.k == row || g.info(u).t.k == column) 
		&& (g.info(g.adj(u)[0]).op == get_row_from_column 
		|| g.info(g.adj(u)[0]).op == get_column_from_row))))  {
		
		//std::cerr << "flipping transpose " << u << std::endl;
		vertex succ = g.adj(u)[0];
		vertex pred = g.inv_adj(u)[0];
		
		type *newu = new type(*(g.info(pred).t.t));
		newu->dim.dim = g.info(pred).t.dim.dim;
		newu->dim.step = g.info(pred).t.dim.step;
		if (newu->k == row)
			newu->k = column;
		else
			newu->k = row;
		type *news = new type(*newu);
		
		vertex_info tmp = g.info(u);
		g.info(u) = g.info(succ);
		g.info(u).op = get_op(*newu, true);
		g.info(succ) = tmp;
		//g.info(succ).op = trans;
		
		g.info(u).t = *newu;
		g.info(succ).t = *news;
		
		news = &g.info(succ).t;
		while (news) {
			if (news->k == column)
				news->k = row;
			else if (news->k == row)
				news->k = column;
			news = news->t;
		}
		
		subgraph* up = g.find_parent(u);
		subgraph* sp = g.find_parent(succ);
		
		if (up)
			remove(up->vertices, u);
		if (sp)
			sp->vertices.push_back(u);
		
		//std::ofstream out("lower97.dot");
		//print_graph(out, g);
		return true;
	}
	return false;
}

bool remove_scalar_transpose(vertex u, graph& g) {
	if (g.info(u).op == trans && g.info(u).t.k == scalar) {
		int pred = g.inv_adj(u)[0];
		vector<vertex> adj(g.adj(u));
		for (unsigned int i = 0; i != adj.size(); ++i) {
			int succ = g.adj(u)[i];
			g.add_edge(pred, succ);
		}
		g.clear_vertex(u);
		//std::cerr << "removing transpose " << u << std::endl;
		return true;
	}
	return false;
}

bool remove_intermediate_temporary(vertex u, graph& g) {
	if (g.info(u).op == temporary && g.inv_adj(u).size() == 1 
		&& g.info(u).t.k == g.info(g.adj(u)[0]).t.k 
		&& g.info(u).t.height == g.info(g.adj(u)[0]).t.height) {
		
		// We can not remove this temporary if the adjacent vertex is
		// going to be lowered later on, breaking the above checks.
		for (int i = 0; i < g.adj(u).size(); ++i) {
			vertex f = g.adj(u)[i];
			if (g.info(f).t.k != scalar && (g.info(f).op == multiply 
						|| g.info(f).op == add || g.info(f).op == subtract))
				return false;
		}
		
		//std::cerr << "removing intermediate temp " << u << std::endl;

		int pred = g.inv_adj(u)[0];
		for (vertex i = 0; i < g.adj(u).size(); ++i) {
			int succ = g.adj(u)[i];
			
			if (g.info(u).t.k == g.info(succ).t.k 
			  && g.info(u).t.height == g.info(succ).t.height) {
				g.add_edge(pred, succ);
				if (is_summation(u, g)) {
					g.find_parent(u)->summations.push_back(succ);
				}
			}
			else {
				g.add_edge(g.adj(u)[0], succ);
				//if (is_summation(u, g)) {
				//	g.find_parent(u)->summations.push_back(succ);
				//}
			}
		}
		
		g.clear_vertex(u);
		
		return true;
	}
	
	return false;
}

bool merge_tmp_output(vertex u, graph& g) {
	// replace tmp -> output
	// with    output
	if ((g.info(u).op == output) && g.inv_adj(u).size() == 1 
			&& g.info(g.inv_adj(u)[0]).op == temporary 
			&& g.find_parent(u) == g.find_parent(g.inv_adj(u)[0]) 
			&& g.info(u).t == g.info(g.inv_adj(u)[0]).t) {

		vertex tmp = g.inv_adj(u)[0];

		//std::cerr << "merge tmp " << tmp << " and output " << u << std::endl;

		vertex pred = g.inv_adj(tmp)[0];
		g.add_edge(pred, u);
		vector<vertex> tmp_adj = g.adj(tmp);
		//std::cerr << "tmp adj size = " << g.adj(tmp).size() << std::endl;
		for (unsigned int i = 0; i != tmp_adj.size(); ++i)
			if (tmp_adj[i] != u) {
				g.add_edge(u, tmp_adj[i]);
				//std::cerr << "adding edge " << u << "->" << tmp_adj[i] << std::endl;
			}

		g.clear_vertex(tmp);

		return true;
	}

	return false;
}

bool merge_gets(vertex u, graph& g)
{
	bool change = false;
	for (unsigned int i = 0; i != g.adj(u).size(); ++i) {
		vertex iv = g.adj(u)[i];
		if (g.info(iv).op == get_row || g.info(iv).op == get_column 
			|| g.info(iv).op == get_row_from_column 
			|| g.info(iv).op == get_column_from_row
			|| g.info(iv).op == get_element) {
			for (unsigned int j = i + 1; j != g.adj(u).size(); ++j) {
				vertex jv = g.adj(u)[j];
				if (g.info(jv).op == g.info(iv).op 
						&& (g.find_parent(jv) == g.find_parent(iv) )) {
					//	|| (! (depends_on(g.find_parent(iv), g.find_parent(jv), g)
					//	       || depends_on(g.find_parent(jv), g.find_parent(iv), g))))) {
					//std::cerr << "merge gets " << u << std::endl;
						
					// copy out edges of jv to iv
					vector<unsigned int> adj_j(g.adj(jv));
					for (unsigned int k = 0; k != adj_j.size(); ++k)
						g.add_edge(iv, adj_j[k]);
	
					g.clear_vertex(jv);

					change = true;
					goto end;
				}//if
			}//for
		}//if
	}//for
	end:
	return change;
}

bool merge_sumto_store(vertex u, graph& g)
{
	if (g.info(u).op == sumto && g.adj(u).size() == 1 
			&& g.find_parent(u) == g.find_parent(g.adj(u)[0])) {
		vertex succ = g.adj(u)[0];
		op_code op = g.info(succ).op;
		if (op == store_row || op == store_column || op == store_element
				|| op == store_add_element || op == store_add_row
				|| op == store_add_column) {
			for (int i = 0; i != g.inv_adj(u).size(); i++) {
				g.add_edge(g.inv_adj(u)[i], succ);
			}
			g.info(succ).op = sumto;
			
			g.clear_vertex(u);
			return true;
		}
	}
	return false;
}

bool move_temporary(vertex u, graph& g)
{
	// move a temporary deeper into a loop nesting
	// when the temporary does not need to be
	// where it is -- this likely only occurs in partitioned
	// programs and this can push a temporary to the threads
	// 
	// -------
	// | pred |
	// |     \|
	// |      \ temporary
	// |      /
	// | succ |
	// --------
	if (g.info(u).op == temporary && g.adj(u).size() == 1 
		&& g.inv_adj(u).size() == 1) {
		
		vertex pred = g.inv_adj(u)[0];
		vertex succ = g.adj(u)[0];
		
		if (g.info(pred).t.k != g.info(succ).t.k 
			|| g.info(pred).t.height != g.info(succ).t.height)
			return false;
		
		if (g.info(succ).op != get_row && g.info(succ).op != get_column
			&& g.info(succ).op != get_row_from_column 
			&& g.info(succ).op != get_column_from_row)
			return false;
		
		if (g.info(pred).op != store_row && g.info(pred).op != store_column
			&& g.info(pred).op != store_add_column
			&& g.info(pred).op != store_add_row)
			return false;
		
		if (g.find_parent(pred) == g.find_parent(succ)) {
			subgraph *up = g.find_parent(u);
			subgraph *pp = g.find_parent(pred);
			
			if (pp->parent == up) {
				g.info(pred).op = temporary;

				// we have just reduced the overall size of the contianer
				// update the container
				string bc = g.info(pred).t.get_highest_column()->dim.dim;
				string br = g.info(pred).t.get_highest_row()->dim.dim;
				string ld = "1";
				if (g.info(pred).t.k != scalar) {
					if (g.info(pred).t.get_lowest_ns()->k == row) 
						ld = br;
					else if (g.info(pred).t.get_lowest_ns()->k == column) 
						ld = bc;
				}
				
				type *t = &g.info(pred).t;
				while (t) {
					t->dim.base_cols = bc;
					t->dim.base_rows = br;
					t->dim.lead_dim	= ld;
					t = t->t;
				}	
				
				for (unsigned int i = 0; i < g.adj(succ).size(); ++i)
					g.add_edge(pred,g.adj(succ)[i]);
				
				g.clear_vertex(u);
				g.clear_vertex(succ);

				//std::cout << prnt_detail(&g.info(pred).t) << "\n";

				return true;
			}
		}
	}
	return false;
}
/////////////////////////////////////// END REWRITERS /////////////////////////////

