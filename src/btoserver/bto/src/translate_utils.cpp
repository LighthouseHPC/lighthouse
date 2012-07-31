#include <map>
#include <deque>
#include <iostream>
#include <fstream>
#include "boost/next_prior.hpp"
#include "build_graph.hpp"

using namespace std;

static std::string var_name = "aijklpqrstabcdefgh";
extern std::string precision_type;

int depth(subgraph* sg)
{
        if (sg == 0)
            return 0;
        else
            return 1 + depth(sg->parent);
}

bool ancestor(subgraph* a, subgraph* p)
{
    if (p == 0)
        return false;
    else if (p->parent == a)
        return true;
    else 
        return ancestor(a, p->parent);
}

subgraph* get_child_ancestor(subgraph* a, subgraph* c)
{
    if (c && c->parent == a)
        return c;
    else
        return get_child_ancestor(a, c->parent);
}



void topo_sort_r(vertex u, graph& g, deque<vertex>& order, vector<bool>& visited) 
{
    visited[u] = true;
    for (unsigned int i = 0; i != g.adj(u).size(); ++i) {
        vertex v = g.adj(u)[i];
        if (!visited[v])
            topo_sort_r(v, g, order, visited);
    }
    order.push_front(u);
}

void topo_sort(graph& g, deque<vertex>& order) 
{
    vector<bool> visited(g.num_vertices(), false);
    for (vertex i = 0; i != g.num_vertices(); ++i)
        if (! visited[i])
            topo_sort_r(i, g, order, visited);
}

int toponum = 0;

void order_subgraphs(deque<vertex>& order, 
        map<vertex,subgraph*>& new_sub, 
        map<vertex,vertex>& new_old,
        subgraph* current,
        vector<vertex> const& vertices,
        vector<subgraph*> const& subgraphs,
        graph& g)
{
    // create the graph to sort
    graph sg;
    map<vertex,vertex> old_new;
    //std::cerr << "starting graph creation " << vertices.size() << " " << subgraphs.size() << std::endl;
    {
        map<subgraph*,vertex> sub_new;
        // create the vertices
        for (unsigned int i = 0; i != vertices.size(); ++i) {
            vertex old = vertices[i];
            vertex n = sg.add_vertex(g.info(old));
            old_new[old] = n;
            new_old[n] = old;
        }
        for (unsigned int i = 0; i != subgraphs.size(); ++i) {
            vertex_info vi;
            vertex n = sg.add_vertex(vi);
            new_sub[n] = subgraphs[i];
            sub_new[subgraphs[i]] = n;
        }  
        //std::cerr << sg.num_vertices() << " vertices added" << std::endl;
        // create the edges
        for (vertex u = 0; u != g.num_vertices(); ++u) {
            for (unsigned int i = 0; i != g.adj(u).size(); ++i) {
                vertex v = g.adj(u)[i];
                // normal edges in this subgraph
                if (g.find_parent(u) == current
                        && g.find_parent(v) == current)
                    sg.add_edge(old_new[u],old_new[v]);

                // edges from nested subgraph to this subgraph
                if (ancestor(current, g.find_parent(u))
                        && g.find_parent(v) == current)
                    sg.add_edge(sub_new[get_child_ancestor(current, g.find_parent(u))], old_new[v]);

                // edges from this subgraph to nested subgraph
                if (g.find_parent(u) == current
                		&& ancestor(current, g.find_parent(v)))
                	sg.add_edge(old_new[u], sub_new[get_child_ancestor(current, g.find_parent(v))]);

                // edges from one nested subgrpah to another
                if (g.find_parent(u) != g.find_parent(v)
                        && g.find_parent(u) && g.find_parent(u)->parent == current
                        && g.find_parent(v) && g.find_parent(v)->parent == current)
                    sg.add_edge(sub_new[g.find_parent(u)], sub_new[g.find_parent(v)]);

            }//for
        }//for
    }//create graph
    //std::cerr << "finished graph creation" << std::endl;  

#if 0
    { 
    	std::cout << toponum << "\n";
        std::ofstream fout(string("topo" + boost::lexical_cast<string>(toponum++) + ".dot").c_str());
        print_graph(fout, sg);
    }
#endif

    // topologically sort the graph
    topo_sort(sg, order);
    //std::cerr << "finished topo sort" << std::endl;
}

string expr_of(vertex u, graph& g, subgraph* cur)
{
    if (g.find_parent(u) != cur
            && g.find_parent(u)
            && std::find(g.find_parent(u)->summations.begin(),
                g.find_parent(u)->summations.end(),
                u) != g.find_parent(u)->summations.end()) {
			
        return "t" + boost::lexical_cast<string>(u);
    } else {

        switch (g.info(u).op) {
            case trans:
                assert(g.inv_adj(u).size() == 1);
                return expr_of(g.inv_adj(u)[0], g, cur);
            case negate_op:
                assert(g.inv_adj(u).size() == 1);
                return "(-" + expr_of(g.inv_adj(u)[0], g, cur) + ")";
            case add:
                assert(g.inv_adj(u).size() == 2);
                return "(" + expr_of(g.inv_adj(u)[0], g, cur) + "+" + expr_of(g.inv_adj(u)[1], g, cur) + ")";
            case subtract:
                assert(g.inv_adj(u).size() == 2);
                return "(" + expr_of(g.inv_adj(u)[0], g, cur) + "-" + expr_of(g.inv_adj(u)[1], g, cur) + ")";
            case multiply:
                assert(g.inv_adj(u).size() == 2);
                return "(" + expr_of(g.inv_adj(u)[0], g, cur) + "*" + expr_of(g.inv_adj(u)[1], g, cur) + ")";
            case get_element: {
				string indx;
				indx.assign(var_name,depth(g.find_parent(u)),1);
				
				if (g.info(g.inv_adj(u)[0]).op == get_row_from_column 
					|| g.info(g.inv_adj(u)[0]).op == get_column_from_row)
					indx += "*"+g.info(g.inv_adj(u)[0]).t.dim.step;
				
            	
            	if (g.info(g.inv_adj(u)[0]).op == output || g.info(g.inv_adj(u)[0]).op == input)
            		return g.info(g.inv_adj(u)[0]).label + "[" + indx +"]";
            	
				vertex pred = g.inv_adj(u)[0];
				if (g.info(pred).t.k == scalar && g.adj(pred).size() > 1) {
					for (int i = 0; i < g.adj(pred).size(); ++i) {
						vertex s = g.adj(pred)[i];
						if (s == u) continue;
						
						if (g.info(s).op == input || g.info(s).op == output)
							return g.info(s).label + "[" + indx + "]";
					}
				}
            	return "t" + boost::lexical_cast<string>(pred) 
            		+ "[" + indx + "]";
            }
            case store_element:
            case store_add_element: {
            	char itr = var_name[depth(g.find_parent(u))];
            	for (int i = 0; i != g.adj(u).size(); ++i) {
            		if (g.info(g.adj(u)[i]).op == output) {
            			return g.info(g.adj(u)[i]).label + "[" + itr + "]";
            		}
            	}

            	return "t" + boost::lexical_cast<string>(g.adj(u)[0]) 
            		+ "[" + itr + "]";
            	//return "t" + boost::lexical_cast<string>(u);
            
            }
            case get_row:
            case get_column:
			case get_row_from_column:
			case get_column_from_row:
            case store_row:
            case store_column:
			case store_add_row:
			case store_add_column:
            case temporary:
                return "t" + boost::lexical_cast<string>(u);
            case input:
            case output:
                return g.info(u).label;
            case sumto:
            	return "t" + boost::lexical_cast<string>(u);
            default:
                return "?";
        }
    }
}

string expr_of_noPtr(vertex u, graph& g, subgraph* cur, std::map<unsigned int, 
					 std::pair<string, string> > &indexMap)
{
    if (g.find_parent(u) != cur
		&& g.find_parent(u)
		&& std::find(g.find_parent(u)->summations.begin(),
					 g.find_parent(u)->summations.end(),
					 u) != g.find_parent(u)->summations.end()) {
			return "t" + boost::lexical_cast<string>(u);
		} else {
			
			switch (g.info(u).op) {
				case trans:
					assert(g.inv_adj(u).size() == 1);
					return expr_of_noPtr(g.inv_adj(u)[0], g, cur, indexMap);
				case negate_op:
					assert(g.inv_adj(u).size() == 1);
					return "(-" + expr_of_noPtr(g.inv_adj(u)[0], g, cur, indexMap) + ")";
				case add:
					assert(g.inv_adj(u).size() == 2);
					return "(" + expr_of_noPtr(g.inv_adj(u)[0], g, cur, indexMap) + "+" + expr_of_noPtr(g.inv_adj(u)[1], g, cur, indexMap) + ")";
				case subtract:
					assert(g.inv_adj(u).size() == 2);
					return "(" + expr_of_noPtr(g.inv_adj(u)[0], g, cur, indexMap) + "-" + expr_of_noPtr(g.inv_adj(u)[1], g, cur, indexMap) + ")";
				case multiply:
					assert(g.inv_adj(u).size() == 2);
					return "(" + expr_of_noPtr(g.inv_adj(u)[0], g, cur, indexMap) + "*" + expr_of_noPtr(g.inv_adj(u)[1], g, cur, indexMap) + ")";
				case get_element: {
					char itr = var_name[depth(g.find_parent(u))];
					map<vertex,pair<string,string> >::iterator index_found = indexMap.find(g.inv_adj(u)[0]);
					if (index_found != indexMap.end()) {
						return indexMap[g.inv_adj(u)[0]].second + indexMap[g.inv_adj(u)[0]].first + "[" + itr +"]";
					} else if (g.info(g.inv_adj(u)[0]).label.compare("") == 0) {
						std::cerr << "bad index in expr_of" << endl;
					} else {
						return g.info(g.inv_adj(u)[0]).label + "[" + itr + "]";
					}
					
				}
				case store_element:
				case store_add_element: {
				
					char itr = var_name[depth(g.find_parent(u))];
					vertex succ = g.adj(u)[0];
					for (int i = 0; i != g.adj(u).size(); ++i) {
						if (g.info(g.adj(u)[i]).op == output) {
							succ = g.adj(u)[i];
							break;
						}
					}
					return indexMap[succ].second + indexMap[succ].first + "[" + itr +"]";
				}
				case get_row:
				case get_column:
				case get_row_from_column:
				case get_column_from_row:
				case store_row:
				case store_column:
				case store_add_row:
				case store_add_column:
				case temporary:
					return "t" + boost::lexical_cast<string>(u);
				case input:
				case output:
					return g.info(u).label;
				case sumto:
					return "t" + boost::lexical_cast<string>(u);
				default:
					return "?";
			}
		}
}


string type_to_c(type t, bool ptr = false) 
{
    switch (t.k) {
    case unknown: return "";
    case scalar: 
      if (ptr)
        return precision_type + string("*");
      else
        return precision_type;
    case row:
    case column: return precision_type + string("*");
    default: return "";
    }
}

string type_to_noPtr(type &t, string name,  bool ptr = false)
{
    switch (t.k) {
    case unknown: return "";
    case scalar: 
      if (ptr)
        return precision_type + string("* ") + name;
      else
        return precision_type + " " + name;
    case row:
    case column: {
		string s = precision_type + " " + name + "[" + t.dim.dim + "]";
		//kind k = t.k;
		type *u = t.t;
		while (u) {
			if (u->k == scalar) {
				break;
			} else {
				s += "[" + u->dim.dim + "]";
			}
			u = u->t;
		}
		return s;
	}
    default: return "";
    }
}

string container_size(vertex v, graph const& g) {
	string high, low = "";
	high = g.info(v).t.dim.dim;
	
	type *t = g.info(v).t.t;
	while (t) {
		if (t->k == scalar)
			break;
		if (t->k != g.info(v).t.k) {
			low = "*" + t->dim.dim;
			break;
		}
		t = t->t;
	}
	
	return high + low;
}

string container_size_type(type &tt) {
	string high, low = "";
	high = tt.dim.dim;
	
	type *t =tt.t;
	while (t) {
		if (t->k == scalar)
			break;
		if (t->k != tt.k) {
			low = "*" + t->dim.dim;
			break;
		}
		t = t->t;
	}
	
	return high + low;
}

string size_params(type &t) 
{
	string tmp;
    switch (t.k) {
        case unknown: return "";
        case scalar: return "";
        case column:
        	tmp = "int " + t.dim.dim + ", ";
        	if (t.t && t.t->k == row)
        		return tmp + "int " + t.t->dim.dim + ", ";
        	return tmp; 
        case row:
        	tmp = "int " + t.dim.dim + ", ";
        	if (t.t && t.t->k == column)
        		return "int " + t.t->dim.dim + ", " + tmp;
        	return tmp;
        default: return "";
    }
}

string size_params_no_type(type &t) 
{
	string tmp;
    switch (t.k) {
        case unknown: return "";
        case scalar: return "";
        case column:
        	tmp = t.dim.dim + ", ";
        	if (t.t && t.t->k == row)
        		return tmp + t.t->dim.dim + ", ";
        	return tmp; 
        case row:
        	tmp = t.dim.dim + ",";
        	if (t.t && t.t->k == column)
        		return t.t->dim.dim + ", " + tmp;
        	return tmp;
        default: return "";
    }
}

//generates the function args, i.e. (double *A, int A_ncols....)
//ideally this code would also be shared with translate_utils.cpp, but it's not yet
//typeinfo = true : types will be added
//diffout = used so blas can send to different outputs, otherwise ""
std::string function_args(map<string,type*> const &inputs, 
						map<string, type*> const &outputs, 
						map<string,pair<vertex,type> > &data, 
						bool typeinfo,
						//std::string diffall,
						std::string diffout,
						bool noPtr) {
	std::string args = "(";
  	for (map<string,type*>::const_iterator i = inputs.begin(); i != inputs.end(); ) {
  		// This is checking for inouts, they will be handled with outputs.
  		// The inouts could be checked for this instead of inputs which may
  		// be more correct.
  		if (outputs.find(i->first) != outputs.end()) {
			++i;
			continue;
		}
		string name = i->first;
		/*if (i->second->k != scalar) {
			name = diffall + i->first;
		} else {
			name = i->first;
		}*/
  		
		if (typeinfo) {
			args += size_params(*i->second);
			if (noPtr) {
				args += type_to_noPtr(*i->second,name);
			} else {
				args += type_to_c(*i->second) + " " + name;
			}
		} else {
			args += size_params_no_type(data[i->first].second) + name;
		}
    	++i;
    	while (i != inputs.end() && outputs.find(i->first) != outputs.end())
    		++i;
    	if (i != inputs.end()) {
			args += ", ";
		}
  	}
  	
  	if (inputs.begin() != inputs.end()) {
		args += ", ";
	}
	
  	for (map<string,type*>::const_iterator i = outputs.begin(); i != outputs.end(); ++i) { 
		string name;
		if (i->second->k != scalar) {
			if (typeinfo) {
				//name = diffall + i->first;
				name = i->first;
			} else {
				//name = diffall + diffout + i->first;
				name = diffout + i->first;
			}
		} else {
			name = diffout + i->first;
		}
		if (typeinfo) {
			if (noPtr) {
				args += size_params(*i->second);
				args += type_to_noPtr(*i->second, name, true);
			} else {
				args += size_params(*i->second);
				args += type_to_c(*i->second, true) + " " + name;
			}
			if (i->second->k == scalar) {
				args += "_ptr";
			}
		} else {
			if (i->second->k == scalar) {
				args += "&";
			}
			args += size_params_no_type(data[i->first].second) + name;
		}
	    if (boost::next(i) != outputs.end()) {
			args += ", ";
		}
  	}
	
  	args += ")";
	return args;
}
