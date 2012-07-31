#include "translate_to_code.hpp"

using namespace std;
extern std::string precision_type;
static std::string var_name = "aijklpqrstabcdefgh";
map<string,string> step_mp;

void translate_graph(ostream& out,
		     subgraph* current,
		     vector<vertex> const& vertices,
		     vector<subgraph*> const& subgraphs,
		     graph& g)
{
	
	// OpenMP expects the loop test to use < instead of !=.
	// The Intel compiler doesn't like loop indices of type unsigned int.
	
  	char vn = var_name[depth(current)];
  	
  	// if a partition was introduced, handle uneven partitions
  	string step = current->step;
  	string iterations = current->iterations;
  	if (step.find("$$") == 0) {
  		step.erase(0,2);
  		step = "__s" + step;
  	}
  	if (iterations.find("$$") == 0) {
  		iterations.erase(0,2);
  		iterations = "__m" + iterations;
  	}
  
	out << "for (int " << vn << " = 0;";
	out << vn << " < " << iterations
		<< "; " << vn << "+=" << step << ") {\n";
	
	if (current->step.find("$$") == 0) {
		step = current->step;
		step.erase(0,2);
		
		out << "int __m" << step << " = " << var_name[depth(current)] << " + __s" << step << " > " 
			<< iterations << " ? " << iterations << " - " << var_name[depth(current)] 
			<< " : __s" << step << ";\n"; 

	}
	
  	//std::cerr << "starting graph translation" << std::endl;
  	// topologically sort the subgraphs
  	deque<vertex> order;
  	map<vertex,subgraph*> new_sub;
  	map<vertex,vertex> new_old;
  	order_subgraphs(order, new_sub, new_old, current, vertices, subgraphs, g);

  	//std::cerr << "declaring variables" << std::endl;

  	// Declare variables  	
  	for (unsigned int i = 0; i != order.size(); ++i) {
    	map<vertex,vertex>::iterator iter = new_old.find(order[i]);
    	if (iter != new_old.end()) {
      		vertex u = iter->second;
      			
      		if (g.adj(u).size() > 0) {
        		//std::cerr << "getting iter height " << height(g.info(u).parent) << std::endl;        
        		char iter = var_name[depth(g.find_parent(u))];
        		//std::cerr << "iter: " << iter << std::endl;
        		switch (g.info(u).op) {
        			case get_row:
        			case get_column: {
          				vertex pred = g.inv_adj(u)[0];
          				string prl = g.info(pred).label;
          				if (g.info(pred).op != input && g.info(pred).op != output)
          					prl = "t" + boost::lexical_cast<string>(pred);
          					
		  				out << precision_type << "* t" << u << " = " << prl
		  				<< " + " << iter << get_next_elem(g.info(pred).t) << ";\n";
	  					break;
					}
        			case store_row:
        			case store_column: {
        				if (u == 63) {
        					std::cout << g.info(u).t.dim.base_rows << "\t"
        							 << g.info(u).t.dim.base_cols << "\n";
        				}
          				vertex succ = g.adj(u)[0];
          				string scl = g.info(succ).label;
          				if (g.info(succ).op != output)
          					scl = "t" + boost::lexical_cast<string>(succ);
          					
	  					out << precision_type << "* t" << u << " = " << scl
	  					<< " + " << iter << get_next_elem(g.info(succ).t) << ";\n";
	  					break;
					}
        			case get_element: {
          				vertex pred = g.inv_adj(u)[0];
          				out << precision_type << " t" << u << " = " 
          					<< expr_of(pred,g,current) << "[" << iter << "];\n";
          				break;
        			}
        			case store_element: 
					case store_add_element: {
						vertex succ = g.adj(u)[0];
          				string target = g.info(succ).label;
          				if (g.info(succ).op != output)
          					target = "t" + boost::lexical_cast<string>(succ);
				        out << precision_type << "& t" << u << " = " << target << "[" 
				        	<< iter << "];\n";
				        break;
					}
        			case temporary:
          				switch (g.info(u).t.k) {
          					case row:
          					case column:
            					out << precision_type << "* t" << u << " = new " 
            						<< precision_type << "[" << container_size(u, g) << "];\n";
            					break;
          					case scalar:
					            out << precision_type << " t" << u << ";\n";
					            break;
				        	default:
				        	 	break;
          				}//switch inside temporary case
          				break;
          			case sumto: {
          				vertex succ = g.adj(u)[0];
          				string scl = "t" + boost::lexical_cast<string>(succ);
          				if (g.info(succ).op == output)
    						scl = g.info(succ).label;
          				
          				if (g.info(u).t.height < g.info(succ).t.height) {
          					// store
          					if (g.info(u).t.k == scalar) {
          						out << precision_type << "& t" << u << " = " << scl;
          						if (g.info(succ).t.k != scalar)
          							out << "[" << iter << "]";
          						out << ";\n";
          					}
          					else {
          						out << precision_type << "* t" << u << " = " << scl
				  					<< " + " << iter << get_next_elem(g.info(succ).t) << ";\n";
          					}
          				}
          				else {
          					if (g.info(u).t.k == scalar) {
	      						if (depth(g.find_parent(u)) > depth(g.find_parent(succ)))
	      							out << precision_type << " &t" << u << " = " << scl;
	      						else
	      							out << precision_type << " t" << u;
	      						out << ";\n";
          					}
	      					else
	      						if (depth(g.find_parent(u)) > depth(g.find_parent(succ)))
	      							out << precision_type << " *t" << u << " = " << scl << ";\n";
	      						else {
	      							out << precision_type << " *t" << u << "= new " 
	      								<< precision_type << "[" << container_size(u,g) << "];\n";
	      						}
          				}
          				
          				if (!been_cleared(u,g,0)) {
          					if (g.info(u).t.k == scalar) {
          						out << "t" + boost::lexical_cast<string>(u) 
          							<< " = 0.0;\n";
          					}
          					else {
          						string cs = container_size(u,g);
          						size_t pos = cs.find("__s",0);
          						while (pos != string::npos) {
          							cs.replace(pos,3,"__m");
          							pos = cs.find("__s",0);
          						}
          						// when i fix the sizes of vertices ($$# -> __s#) i only do top level
          						// of type.  this case goes below the top level of the type
          						//pos = cs.find("$$",0);
          						//while (pos != string::npos) {
          						//	cs.replace(pos,2,"__s");
          						//	pos = cs.find("$$",0);
          						//}
          						out << "for (int ii = 0; ii < " << cs << "; ++ii)\n";
								out << "t" + boost::lexical_cast<string>(u) << "[ii] = 0.0;\n";
          					}
          				}

      					break;
      				}
        			default:
          				break;
        		}//switch	 	
      		}//if
    	}//if
  	}//for
  	//std::cerr << "finished declaring variables" << std::endl;

  	// Do computations and store the results
  	for (unsigned int i = 0; i != order.size(); ++i) {
    	map<vertex,subgraph*>::iterator iter = new_sub.find(order[i]);
    	if (iter != new_sub.end()) {
	    	subgraph* sg = iter->second;
      		translate_graph(out, sg, sg->vertices, sg->subs, g);
    	} 
    	else {
      		map<vertex,vertex>::iterator iter = new_old.find(order[i]);
      		if (iter != new_old.end()) {
        		vertex u = iter->second;
        		switch (g.info(u).op) {
        			case store_element:
          				if (g.inv_adj(u).size() == 1)
            				out << "t" << u << " = " << expr_of(g.inv_adj(u)[0], g, current) << ";\n";
          				break;
        			case store_add_element:
          				if (g.inv_adj(u).size() == 1)
            				out << "t" << u << " += " << expr_of(g.inv_adj(u)[0], g, current) << ";\n";
          				break;
        			case sumto:
        				break;
        				if (g.info(u).t.k == scalar) {
        					if (been_cleared(u, g, 0))
        						out << "t" << u << " += t" << g.inv_adj(u)[0] << ";\n";
        					else
        						out << "t" << u << " = t" << g.inv_adj(u)[0] << ";\n";
        				}
        				break;
        			default:
          				if (g.adj(u).size() > 0 && g.find_parent(u) 
          						&& std::find(g.find_parent(u)->summations.begin(),
                           		g.find_parent(u)->summations.end(),u) 
                           		!= g.find_parent(u)->summations.end()) {
          					vertex succ = g.adj(u)[0];
          					out << "t" << succ << " += " << expr_of(u, g, current) << ";\n";

          				}
          				break;
        		}
      		}
    	}
	}

  	for (unsigned int i = 0; i != vertices.size(); ++i) {
    	vertex u = vertices[i];
    	if (g.info(u).op == output && g.info(u).t.k == scalar) {
    		vertex pred = g.inv_adj(u)[0];
    		
    		// in the single core case if pred is a sumto it is pointing to the output node
    		// any way so this should be skipped
    		if (g.info(pred).op == sumto)
    			continue;
      		
      		string p_label = g.info(pred).label == "" ? "t" + boost::lexical_cast<string>(pred) 
      			: g.info(pred).label;
      		out << g.info(u).label << " = " << p_label << ";" << std::endl;
    	}
  	}

	out << "}\n";
  	//std::cerr << "finished translating graph" << std::endl;
}

void translate_to_cpp(ostream& out,
		    string name,
		    map<string,type*>const& inputs,
		    map<string,type*>const& outputs, 
		    graph& g)
{
	// change all vertex sizes from $$* to s*
	// have to touch all levels of a type because functions like get_next_element use
	// lower levels of a given type
	for (int i = 0; i != g.num_vertices(); i++) {
		type *t = &g.info(i).t;
		while (t) {
			string &s = t->dim.dim;
			if (s.find("$$") == 0) {
				s.erase(0,2);
				s = "__s" + s;
			}
			t = t->t;
		}
	}
	
	step_mp.clear();
	int parallel = 0;
	if (parallel) {
		out << "#include <pthread.h>\n";
		out << "#include <stdlib.h>\n";
		out << "#define NUM_THREADS 4\n";
		
		//data_parallel_init(g.subgraphs, g, out, partition);	
	}
	
  	out << "void " << name << "(";
  	for (map<string,type*>::const_iterator i = inputs.begin(); i != inputs.end(); ++i) {
    	out << type_to_c(*i->second) << " " << i->first;
    	out << size_params(*i->second);
    	if (boost::next(i) != inputs.end())
      		out << ", ";
  	}
  	
  	if (inputs.begin() != inputs.end())
    	out << ", ";
  
  	for (map<string,type*>::const_iterator i = outputs.begin(); i != outputs.end(); ++i) {
  		// This is checking for inouts that have already been passed in.
  		// The inouts could be checked for this instead of inputs which may
  		// be more correct.
  		if (inputs.find(i->first) != inputs.end())
  			continue;
  		
	    out << type_to_c(*i->second, true) << " " << i->first;
	    out << size_params(*i->second);
	    if (boost::next(i) != outputs.end())
      		out << ", ";
  	}
  
  	out << ") {\n";
  	
  	vector<string> def_iters;
  	vector<string> step(10,"500");
  	init_partitions(g.subgraphs, out, def_iters, step);
  	
  	for (unsigned int u = 0; u != g.num_vertices(); ++u) {
    	if (g.find_parent(u) == 0 && (g.adj(u).size() > 0 || g.inv_adj(u).size() > 0)) {
      		switch (g.info(u).op) {
      		case temporary:
  				switch (g.info(u).t.k) {
  					case row:
  					case column:
    					out << precision_type << "* t" << u << " = new " 
    						<< precision_type << "[" << container_size(u, g) << "];\n";
    					break;
  					case scalar:
			            out << precision_type << " t" << u << ";\n";
			            break;
		        	default:
		        	 	break;
  				}//switch inside temporary case
  				break;
  			case sumto: {
  				vertex succ = g.adj(u)[0];
  				string scl = "t" + boost::lexical_cast<string>(succ);
  				if (g.info(succ).op == output)
					scl = g.info(succ).label;
  				
  				int d = (depth(g.find_parent(u)) - depth(g.find_parent(succ)));
  				if (d == 0) {
  					// just point to
  					if (g.info(u).t.k == scalar)
  						out << precision_type << " &t" << u << " = " << scl << ";\n";
  					else
  						out << precision_type << " *t" << u << " = " << scl << ";\n";
  				}
  				else if (d < 0) {
  					// treat as temporary
  					if (g.info(u).t.k == scalar) {
  						out << precision_type << " t" << u << ";\n";
  					}
  					else {
  						out << precision_type << "* t" << u << " = new " 
    						<< precision_type << "[" << container_size(u, g) << "];\n";
  					}
  				}
  				else {
  					std::cout << "WARNING: translate_to_c.cpp: translate_to_cpp(): unexpected graph\n";
  				}
  				
				if (g.info(u).t.k == scalar) {
					out << "t" + boost::lexical_cast<string>(u) << " = 0.0;\n";
				}
				else {
					out << "for (int ii = 0; ii < " << container_size(u, g) << "; ++ii)\n";
					out << "t" + boost::lexical_cast<string>(u) << "[ii] = 0.0;\n";
				}
  				break;
  			}
  			case input:
  			case output:
  				break;
      		default: {
      			std::cout << "WARNING: translate_to_c.cpp: translate_to_cpp(): unexpected op\n";
      		}
      		}
    	}
  	}
  	
  	for (int i = 0; i != g.subgraphs.size(); i++) {
  		subgraph *sg = g.subgraphs[i];
  		translate_graph(out, sg, sg->vertices, sg->subs, g);
  	}
  	
  	out << "}\n";
  	
}

/*
void get_struct_iterators(subgraph *sg, std::vector<subgraph*> &subs, ostream &out) {
	std::vector<subgraph*>::iterator itr = subs.begin();
	for (; itr != subs.end(); itr++) {
		//if ((*itr)->start != 0) {
		//	std::string vn(1,var_name[height((*itr)->start)]);
		//	out << "int " << vn << ";\n";
		//	sg->strct[normal]["int"].insert(vn);	
		//}
		std::set<std::string> &ptr = sg->strct[normal]["int"];
		if (find(ptr.begin(), ptr.end(), (*itr)->iterations) == ptr.end()) {
			out << "int " << (*itr)->iterations << ";\n";
			if (sg == (*itr)->parent)
				sg->strct[step]["int"].insert((*itr)->iterations);
			else
				sg->strct[normal]["int"].insert((*itr)->iterations);
		}
		
		get_struct_iterators(sg, (*itr)->subs, out);
	}	
}
/*
void data_parallel_init(std::vector<subgraph*> subs, graph& g, ostream& out, bool partition) {
	
	std::vector<subgraph*>::iterator i = subs.begin();

	for (; i != subs.end(); i++) {
		subgraph *sg = *i;
		if ((*i)->parallel == data) {
			std::string tid = "t" + boost::lexical_cast<string>(sg);
      		std::string strct = "st_" + tid;
      		std::string strct_t = strct + "_t";
			out << "typedef struct {\n";
		
			//find inputs
			std::vector<vertex>::iterator l = sg->inputs.begin();
			for (;l != sg->inputs.end(); l++) {
///////////////////FIX ME - THIS ACTUALLY NEEDS TO HAPPEN FOR EACH INV_ADJ TO *l
///////////////////AND EVERYTHING BELOW SHOULD BE BASED ON THAT
				vertex v = *l;//g.inv_adj(*l)[0];
				
				string ls = g.info(v).label;
				if (ls.compare("") == 0)
					ls = "t" + boost::lexical_cast<string>(v);
				else
					ls = g.info(v).label;
				
				string type = precision_type + " ";

				if (g.info(v).t.k != scalar_type) {
					type += " *";
				}
					
				out << type << ls << ";\n";
				
				if (find(sg->subs.begin(), sg->subs.end(),g.find_parent(v)) != sg->subs.end()) 
					sg->strct[part][type].insert(ls);
				else
					sg->strct[normal][type].insert(ls);
				
				
				switch (g.info(v).t.k) {
					case unknown:
						std::cout << "WARNING: translate_to_c.cpp: translate_graph: unknown type\n";
						break;
					case scalar_type:
						break;
					case vector_type:
						out << "int " << vector_size(v,g) << ";\n";
						sg->strct[normal]["int"].insert(vector_size(v,g));
						break;
					case matrix:
						if (g.info(v).op == block_m) {
							if (!(g.info(v).nrows.compare("512") == 0 
									|| g.info(v).nrows.compare("128") == 0)) {
								out << "int " << g.info(v).nrows << ";\n";
								sg->strct[normal]["int"].insert(g.info(v).nrows);	
							}
							if (!(g.info(v).ncols.compare("512") == 0 
									|| g.info(v).ncols.compare("128") == 0)) {
								out << "int " << g.info(v).ncols << ";\n";
								sg->strct[normal]["int"].insert(g.info(v).ncols);	
							}
						}
						else {
							out << "int " << g.info(v).nrows << ";\n";
							out << "int " << g.info(v).ncols << ";\n";
							sg->strct[normal]["int"].insert(g.info(v).nrows);
							sg->strct[normal]["int"].insert(g.info(v).ncols);
						}
						break;
					default:
						std::cout << "ERROR: translate_to_c.cpp: translate_graph: bad type ("
								  << v << ": " << g.info(v).t.k << ")\n";
						break;
				}
			}
			
			//find loop variables
			std::vector<subgraph*>::iterator itr = sg->subs.begin();
			// this needs to recursivly go through all children subgraphs
			get_struct_iterators(sg, sg->subs, out);
			
			//find outputs
			// this may need to be its own element of struct subgraph specific to parallel output
			l = sg->subs[0]->outputs.begin();
			for (; l != sg->subs[0]->outputs.end(); l++) {
				out << precision_type << " *" << g.info(g.adj(*l)[0]).label << ";\n";
				sg->strct[part][precision_type+"*"].insert(g.info(g.adj(*l)[0]).label);
			}
			
			out << "}\n";
      		out << strct_t << ";\n";
      		
      		out << "void *" << tid << "(void *mesg) {\n";
      		out << strct_t << " *" << strct << " = (" << strct_t << "*) mesg;\n";
      		std::map<_pstruct, std::map<std::string, std::set<std::string> > >::iterator jtr;
      		for (jtr = sg->strct.begin(); jtr != sg->strct.end(); jtr++) {
      			std::map<std::string, std::set<std::string> >::iterator ltr;
      			for (ltr = jtr->second.begin(); ltr != jtr->second.end(); ltr++) {
	      			std::set<std::string>::iterator ktr;
	      			for (ktr = ltr->second.begin(); ktr != ltr->second.end(); ktr++) {
	      				out << ltr->first << " " << *ktr << " = " << strct << "->" << *ktr << ";\n";
	      			}
      			}
      		}
      			
			translate_graph(out,sg,sg->vertices,sg->subs,g, partition);
			out << "return 0;\n"; //icc ism
			out << "}\n";
			
		}
		
		data_parallel_init(sg->subs, g, out, partition);
	}
}
*/
