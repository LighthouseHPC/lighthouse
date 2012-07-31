#ifndef SYNTAX_HPP
#define SYNTAX_HPP

#include <string>
#include <vector>
#include <map>
#include <set>
#include <iostream>
#include "boost/function.hpp"
#include "boost/lexical_cast.hpp"
#include <utility>

using std::vector;
using std::map;
using std::string;
using std::pair;
using std::set;

#define NUM_THREAD 4
#define STR_NUM_THREAD "NUM_THREADS"

enum op_code { deleted, trans, negate_op, 
	add, subtract, multiply, sum, sumto,
	input, output, temporary,
	get_row, get_column, get_element, 
	get_row_from_column, get_column_from_row, 
	store_row, store_add_row, store_add_column,
	store_column, store_element,
	store_add_element };

enum kind { unknown, row, column, scalar };
enum storage { dense, compressed, coordinate };

/* GRAPH.HPP USES SOME OF THE ABOVE ENUMERATIONS !! */
#include "graph.hpp"

struct dim_info {
	dim_info() : base_rows("?"), base_cols("?"), dim("?"), step("1"),
	lead_dim("?") {}
	dim_info(string br, string bc, string dim) : base_rows(br), base_cols(bc),
	dim(dim), step("1"), lead_dim("1") {}
	dim_info(string br, string bc, string dim, string blks, string lda) :
	base_rows(br), base_cols(bc), dim(dim), step(blks), 
	lead_dim(lda) {}
	
	dim_info(const dim_info &info) : base_rows(info.base_rows), 
	base_cols(info.base_cols), dim(info.dim), step(info.step),
	lead_dim(info.lead_dim) {}
	string dim;
	string step;
	
	//describe unpartitioned base structure (these should not change during partitioning)
	string base_rows;
	string base_cols;
	string lead_dim;
	
	void del() {
		dim.clear();
		step.clear();
		base_rows.clear();
		base_cols.clear();
		lead_dim.clear();
	}
	
};


struct type {
	//for compiler
	type() : k(unknown), s(dense), t(0), height(0),
	dim() {}
	type(kind k, dim_info d) : k(k), dim(d), s(dense), t(0), height(0) { }
	type(kind k, dim_info d, storage s) : k(k), dim(d), s(s), t(0), 
	height(0) { }
	type(kind k, dim_info d, storage s, int height) : k(k), dim(d), s(s),
	t(0), height(height) { }
    
	//for parsing only
	//matrix & vector
	type(string *orien, string data, storage s) : s(s), height(0), 
	dim()
  	{ 
  		if (data.compare("matrix") != 0 && data.compare("vector") != 0) {
  			std::cout << "ERROR: syntax.hpp: type() constructor: expecting matrix\n";
  			exit(-1);
  		}
  		//matrix
  		if (data.compare("matrix") == 0) {	
	  		string o = *orien;
	  		if (o.compare("column") == 0) {
	  			k = row;
	  			height = 2;
	  			
	  			//column
	  			type *clm = new type(column, dim_info(), dense, 1);
	  			t = clm; 
	  			
	  			//scalar
	  			type *scl = new type(scalar, dim_info("???","???","1"), dense, 0);
	  			clm->t = scl;			
	  		}
	  		else if (o.compare("row") == 0) {
	  			k = column;
	  			height = 2;
	  			
	  			//row
	  			type *rw = new type(row, dim_info(), dense, 1);
	  			t = rw; 
				
	  			//scalar
	  			type *scl = new type(scalar, dim_info("???","???","1"), dense, 0);
	  			rw->t = scl;
	  		}
	  		else {
	  			std::cout << "ERROR: syntax.hpp: type structure constructor: unknown orientation\n";
	  		}
	  	}
	  	//vector
	  	else {
	  		string o = *orien;
	  		if (o.compare("row") == 0) {
	  			k = row;
	  			height = 1;
	  			dim.base_rows = "1";
	  			
	  			//scalar
	  			type *scl = new type(scalar, dim_info("1","???","1"), dense, 0);
	  			t = scl;			
	  		}
	  		else if (o.compare("column") == 0) {
	  			k = column;
	  			height = 1;
	  			dim.base_cols = "1";
	  			
	  			//scalar
	  			type *scl = new type(scalar, dim_info("???","1","1"), dense, 0);
	  			t = scl;
	  		}
	  		else {
	  			std::cout << "ERROR: syntax.hpp: type structure constructor: unknown orientation\n";
	  		}
			
	  	}
  	}
  	
	//scalar
	type(kind k) : k(k), s(dense), t(0), height(0), 
	dim("1","1","1","1","1") { }
	
	//copy
	type(const type &ot) : k(ot.k), s(ot.s), height(ot.height),
	dim(ot.dim)
	{
		if (ot.t) {
			t = new type(*(ot.t));
		}
		else
			t = 0;
	}	
	
	// assignment operator
	type& operator=(const type &ot) {
		k = ot.k;
		s = ot.s;
		dim = ot.dim;
		height = ot.height;
		
		if (t)
			t->del();
		
		if (ot.t) {
			t = new type(*(ot.t));
		}
		else
			t = 0;
		
		return *this;
	}	
	
	kind k;
	storage s;
	dim_info dim;
	type *t;
	int height;
	
	
	// util functions //
	type *get_highest_row() {
		if (this->k == scalar)
			return this;
		
		type *t = this;
		while (t) {
			if (t->k == row || t->k == scalar)
				return t;
			t = t->t;
		}
		std::cout << "ERROR: syntax.hpp: get_highest_row(): unexpected type\n";
		return t;
	}
	
	type *get_highest_column() {
		if (this->k == scalar)
			return this;
		
		type *t = this;
		while (t) {
			if (t->k == column || t->k == scalar)
				return t;
			t = t->t;
		}
		std::cout << "ERROR: syntax.hpp: get_highest_column(): unexpected type\n";
		return t;
	}
	
	type *get_lowest_ns() {
		if (this->k == scalar)
			std::cout << "ERROR: syntax.hpp: get_lowest_ns(): unexpected type\n";
		
		type *t = this;
		while (t) {
			if (t->t->k == scalar)
				return t;
			t = t->t;
		}
		std::cout << "ERROR: syntax.hpp: get_lowest_ns(): unexpected type\n";
		return NULL;
	}
	
	int whichPartition(string dim, kind k) {
		// a container above will call this to
		// find which container below is its matching
		// partition and return the height of that
		// partition.
		if (dim.compare("1") == 0)
			return -1;
		
		type *t = this;
		while (t) {
			if (t->k == k && t->dim.dim.compare(dim) == 0)
				return t->height;
			t = t->t;
		}
		return -1;
	}
	
	bool beenPartitioned() {
		type *t = this;
		int r, c;
		r = c = 0;
		while (t) {
			if (t->k == row) 
				++r;
			else if (t->k == column)
				++c;
			
			if (r > 1 || c > 1) {
				return true;
			}
			t = t->t;
		}
		return false;
	}
	
	int num_rows() {
		int ret = 0;
		type *t = this;
		
		while (t) {
			if (t->k == row)
				ret++;
			if (t->k == scalar)
				break;
			t = t->t;			
		}
		return ret;
	}
	
	int num_cols() {
		int ret = 0;
		type *t = this;
		
		while (t) {
			if (t->k == column)
				ret++;
			if (t->k == scalar)
				break;
			t = t->t;
		}
		return ret;
	}
	
	void del() {
		type *n = this->t;
		type *t;
		delete this;
		while (n) {
			t = n;
			n = n->t;
			t->dim.del();
			delete t;
		}
	}
	
}; 

inline bool operator==(type t1, type t2) {
	//this may need to be recursive now...
	return t1.k == t2.k && t1.s == t2.s;
}


enum eval_choice { defer, evaluate };
enum param_access { once, many };
enum computation_kind { inseparable, decouple };

string type_to_string(type t);
string op_to_string(op_code op);
string op_to_name(op_code op);

typedef unsigned int vertex;

struct vertex_info
{
	vertex_info() : eval(defer), trivial(true) { }
	vertex_info(op_code op)
    : op(op), eval(defer), trivial(true) 
	{ }
	vertex_info(op_code op, string label)
    : op(op), eval(defer), trivial(true), label(label) 
	{ }
	vertex_info(type t, op_code op, string label) 
    : t(t), op(op), label(label), eval(defer), trivial(true)
	{ }
	vertex_info(type t, op_code op, string label, double v)
    : t(t), op(op), val(v), label(label), eval(defer), trivial(true) 
	{ }

	type t;
	op_code op;
	double val;		// if a constant is provided store value
	string label;
	eval_choice eval;
	int algo;
	bool trivial; // some computation happens, not just I/O
	
	string to_string(vertex i) const;
	
	void del() {
		if (t.t)
			t.t->del();
		label.clear();
	}
};

typedef Graph<vertex_info> graph;
typedef boost::function<bool(vertex,graph&)> rewrite_fun;
typedef boost::function<int(vertex,graph&,std::pair<int,int>&)> optim_fun;
typedef boost::function<set<pair<int,int> >(vertex,graph&,std::set<int>&)> optim_fun_chk;
typedef boost::function<type(vertex u, vector<vertex>, graph&)> return_type_fun;
typedef boost::function<vector<param_access>(vector<vertex>, vertex u, graph&)> param_access_fun;
typedef boost::function<bool(vertex, graph &)> constraint_fun;

struct algo {
	algo(op_code op, return_type_fun rt, computation_kind comp, param_access_fun pa, type arg, 
		 rewrite_fun r,bool w, constraint_fun cf)
    : op(op), return_type(rt), comp(comp), param_accesses(pa), refiner(r), 
	does_work(w), additionalConstraints(cf) {
		param_types.push_back(arg);	
	}
	algo(op_code op, return_type_fun rt, computation_kind comp, param_access_fun pa, type arg1,  
		 type arg2, rewrite_fun r, bool w, constraint_fun cf)
    : op(op), return_type(rt), comp(comp), param_accesses(pa), refiner(r), 
	does_work(w), additionalConstraints(cf) {
		param_types.push_back(arg1);
		param_types.push_back(arg2);
	}
	op_code op;
	return_type_fun return_type;
	computation_kind comp;
	vector<type> param_types;
	param_access_fun param_accesses;
	rewrite_fun refiner;
	bool does_work;
	constraint_fun additionalConstraints;
};

typedef std::pair<const std::string, type*> param;

struct expr { 
	virtual void print() = 0;
	virtual vertex to_graph(map<string, vertex>& env, map<string,type*>const& inputs, 
							map<string,type*>const& outputs, graph& g) = 0;
	virtual ~expr() { }
};

struct operation : public expr {
	op_code op;
	vector<expr*> operands;
	
	operation(op_code op, expr* e) : op(op) {
		operands.push_back(e);
	}
	
	operation(op_code op, expr* e1, expr* e2) : op(op) {
		operands.push_back(e1);
		operands.push_back(e2);
	}
	
	virtual vertex to_graph(map<string, vertex>& env, map<string,type*>const& inputs, 
							map<string,type*>const& outputs, graph& g);
	
	virtual void print();
	
};

struct variable : public expr {
	variable(std::string* name) : name(*name) { }
	std::string name;
	void print();
	virtual vertex to_graph(map<string, vertex>& env, map<string,type*>const& inputs, 
							map<string,type*>const& outputs, graph& g);
};

struct scalar_in : public expr {
	scalar_in(double val) : val(val) { }
	double val;
	void print();
	virtual vertex to_graph(map<string, vertex>& env, map<string,type*>const& inputs, 
							map<string,type*>const& outputs, graph& g);
};

struct stmt {
	stmt(std::string* lhs, expr* rhs) : lhs(*lhs), rhs(rhs) { }
	std::string lhs;
	expr* rhs;
	void print();
};

void print_program(vector<stmt*>& p);

// partition stuff
void make_partitions(graph &g);
pair<std::string, std::string> partition_to_c(vertex u, graph &g, std::ostream& out);

#endif // SYNTAX_HPP
