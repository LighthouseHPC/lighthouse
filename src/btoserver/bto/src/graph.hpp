#ifndef GRAPH_HPP
#define GRAPH_HPP

#include <iostream>
#include <iterator>
#include <string>
#include <vector>
#include <map>
#include <set>
#include <cassert>

using std::vector;
using std::map;
using std::string;

struct subgraph;


template<class V>
class Graph {
public:
  typedef vector<unsigned int>::const_iterator adj_iter;
  typedef vector<unsigned int>::const_iterator inv_adj_iter;

  vector<unsigned int> const& adj(unsigned int i) const {
    assert(i < outs.size());
    return outs[i];
  }
  vector<unsigned int> const& inv_adj(unsigned int i) const { 
    assert(i < ins.size());
    return ins[i];
  }
  V& info(unsigned int i) { return _info[i]; }
  V const& info(unsigned int i) const { return _info[i]; }

  std::size_t num_vertices() const { return _info.size(); }

  int add_vertex(V const& v) {
    int n = _info.size();
    _info.push_back(v);
    ins.push_back(vector<unsigned int>());
    outs.push_back(vector<unsigned int>());
    return n;
  }
  bool add_inputs(vector<subgraph*> &subs, subgraph* end, unsigned int v);
  bool remove_inputs(vector<subgraph*> &subs, subgraph* end, unsigned int v);
  void add_edge(unsigned int u, unsigned int v);
  void remove_edges(unsigned int u, unsigned int v);
  void clear_vertex(unsigned int u);
  
  int merge(subgraph* sg1, subgraph* sg2);

  void register_iter(string iter) {
    if (iter_rep.find(iter) == iter_rep.end())
      iter_rep[iter] = iter;
  }
  
  string get_iter_rep(string iter) {
	  map<string,string>::iterator ret = iter_rep.find(iter);

	  if (ret != iter_rep.end())
	  	return ret->second;
	  return iter;
  }
  
void merge_iters(string iter1, string iter2) {
	string r1 = iter_rep[iter1];
	string r2 = iter_rep[iter2];
	if (r1 != r2) {
		vector<string> keys;
		for (map<string,string>::iterator i = iter_rep.begin(); i != iter_rep.end(); ++i) {
			keys.push_back(i->first);
		}
		for (vector<string>::iterator i = keys.begin(); i != keys.end(); ++i) {
			if (iter_rep[*i] == r2)
				iter_rep[*i] = r1;
		}
	}
}

  bool is_equal(string iter1, string iter2) {
    register_iter(iter1);
    register_iter(iter2);
    return iter_rep[iter1] == iter_rep[iter2];
  }
  bool mergeable(unsigned int u, unsigned int v);

  Graph() { }

  Graph(const Graph<V>& g)
    : _info(g._info), outs(g.outs), ins(g.ins), iter_rep(g.iter_rep)
  {
    for (unsigned int i = 0; i != g.subgraphs.size(); ++i)
      subgraphs.push_back(new subgraph(*g.subgraphs[i]));
  }
  
  void del() {
	  for (int i = 0; i != num_vertices(); i++) {
		  info(i).del();
	  }
	  
	  for (int i = 0; i != subgraphs.size(); i++) {
		  this->subgraphs[i]->del();
	  }
	  subgraphs.clear();
	  _info.clear();
	  outs.clear();
	  ins.clear();
	  iter_rep.clear();
	  delete this;
  }

  subgraph* find_parent(unsigned int v);

private:
  vector< V > _info;
  vector< vector<unsigned int> > outs;
  vector< vector<unsigned int> > ins;
  map<string,string> iter_rep; // representative for iterations (key, represntative)
public:
  vector<subgraph*> subgraphs;
};

extern int subgraphUniqueID;
struct subgraph {

	template<class V>
	subgraph(std::string n, subgraph* p, Graph<V>& g, std::string iters, 
			 std::string s)
    : name(n), parent(p), iterations(iters), step(s) {
		uid = subgraphUniqueID;
		subgraphUniqueID++;
		if (p == 0) {
			g.subgraphs.push_back(this);
		} else {
			p->subs.push_back(this);
		}
		g.register_iter(iters);
	}
	
	subgraph(const subgraph& g, subgraph* p = 0)
    : name(g.name), vertices(g.vertices), summations(g.summations),
	outputs(g.outputs), parent(p), iterations(g.iterations),
	inputs(g.inputs), step(g.step), uid(g.uid)
	{
		for (unsigned int i = 0; i != g.subs.size(); ++i)
			subs.push_back(new subgraph(*g.subs[i], this));
	}
  
	subgraph* find_parent(unsigned int u)
	{
		for (unsigned int i = 0; i != subs.size(); ++i) {
			subgraph* p = subs[i]->find_parent(u);
			if (p) return p;
		}
		
		vector<unsigned int>::iterator i = find(vertices.begin(), vertices.end(), u);
		if (i == vertices.end())
			return 0;
		else
			return this;
	}
  
	int depth() {
		if (parent == 0) 
			return 1;
		else 
			return 1 + parent->depth();
	}
	
	void del() {
		for (int i = 0; i != subs.size(); i++) {
			subs[i]->del();
		}
		name.clear();
		vertices.clear();
		subs.clear();
		summations.clear();
		outputs.clear();
		inputs.clear();
		iterations.clear();
		step.clear();
		delete this;
	}
	
	std::string name;
	int uid;						// unique identifier
	vector<unsigned int> vertices;
	vector<subgraph*> subs;
	vector<unsigned int> summations; // which nodes are being summed up
	vector<unsigned int> outputs; // which nodes are the "output" of this subgraph
	vector<unsigned int> inputs;		// which nodes are the input of this subgraph, including
	// nodes that are inputs of children subgraphs but not
	// physically present in this subgraph
	subgraph* parent;
	std::string iterations;
	std::string step;
};


template<class V>
subgraph* Graph<V>::find_parent(unsigned int u)
{
  for (unsigned int i = 0; i != subgraphs.size(); ++i) {
    subgraph* p = subgraphs[i]->find_parent(u);
    if (p) return p;
  }
  return 0;
}

template<class T>
void remove(vector<T>& vec, T const& x)
{
  typename vector<T>::iterator i = std::find(vec.begin(), vec.end(), x);
  if (i != vec.end()) 
    vec.erase(i);
}

template<class T>
void append(vector<T>& v1, vector<T> const& v2)
{
  v1.insert(v1.end(), v2.begin(), v2.end());
}


template<class V>
void Graph<V>::clear_vertex(unsigned int u)
{
  info(u).algo = 0;
  info(u).op = deleted;
  
  vector<unsigned int> preds(inv_adj(u));
  for (unsigned int i = 0; i != preds.size(); ++i)
    remove_edges(preds[i], u);
  vector<unsigned int> succs(adj(u));
  for (unsigned int i = 0; i != succs.size(); ++i)
    remove_edges(u, succs[i]);
  subgraph* p = this->find_parent(u);
  if (p) {
    remove(p->summations, u);
    remove(p->outputs, u);
    remove(p->vertices, u);
  }
}

// Put everything in sg1 into sg2
template<class V>
int Graph<V>::merge(subgraph* sg1, subgraph* sg2)
{
  if (sg1 != sg2
      && sg1->parent == sg2->parent
      && is_equal(sg1->iterations, sg2->iterations)) {

#if 0
    // Change the vertex parent pointers
    for (unsigned int i = 0; i != sg1->vertices.size(); ++i)
      info(sg1->vertices[i]).parent = sg2;
#endif

    // Change the subgraph parent pointers
    for (unsigned int i = 0; i != sg1->subs.size(); ++i)
      sg1->subs[i]->parent = sg2;

    append(sg2->vertices, sg1->vertices);
    append(sg2->summations, sg1->summations);
    append(sg2->outputs, sg1->outputs);
    append(sg2->subs, sg1->subs);

    vector<subgraph*>* parent_subs;
    if (sg1->parent == 0)
      parent_subs = &subgraphs;
    else
      parent_subs = &(sg1->parent->subs);
    remove(*parent_subs, sg1);

    //delete sg1;
    return sg2->uid;
  } else
    return -1;
}


template<class V>
bool Graph<V>::mergeable(unsigned int u, unsigned int v) {
  subgraph* sg1 = 0, *sg2 = 0;
  sg1 = find_parent(u);
  sg2 = find_parent(v);
  return (sg1 != sg2
	  && sg1->parent == sg2->parent
	  && is_equal(sg1->iterations, sg2->iterations));
}

template<class V>
bool Graph<V>::add_inputs(vector<subgraph*> &subs, subgraph* end, unsigned int v) {

	vector<subgraph*>::iterator itr = std::find(subs.begin(), subs.end(), end);

	if (itr != subs.end()) {
		//if (end->inputs.end() == find(end->inputs.begin(), end->inputs.end(), v))
			end->inputs.push_back(v);
		return true;
	}
	
	for (itr = subs.begin(); itr != subs.end(); itr++) {
		if (add_inputs((*itr)->subs, end, v)) {
			//if ((*itr)->inputs.end() == find((*itr)->inputs.begin(), (*itr)->inputs.end(),v))
				(*itr)->inputs.push_back(v);
			return true;
		}
	}
	return false;
}

template<class V>
bool Graph<V>::remove_inputs(vector<subgraph*> &subs, subgraph* end, unsigned int v) {

	vector<subgraph*>::iterator itr = std::find(subs.begin(), subs.end(), end);
	vector<unsigned int>::iterator tmp;
	
	if (itr != subs.end()) {
		tmp = find(end->inputs.begin(), end->inputs.end(), v);
		if (tmp != end->inputs.end()) 
			end->inputs.erase(tmp);
		return true;
	}
	
	for (itr = subs.begin(); itr != subs.end(); itr++) {
		if (remove_inputs((*itr)->subs, end, v)) {
			tmp = find((*itr)->inputs.begin(), (*itr)->inputs.end(),v);
			if (tmp != (*itr)->inputs.end())
				(*itr)->inputs.erase(tmp);
			return true;
		}
	}
	return false;
}

template<class V>
void Graph<V>::add_edge(unsigned int u, unsigned int v) {
    assert(u < num_vertices());
    assert(v < num_vertices());
    outs[u].push_back(v);
    ins[v].push_back(u);
    
    //update inputs vector for each subgraph
    subgraph* start = find_parent(u);
    subgraph* end = find_parent(v);
    
    if (start == 0)
    	add_inputs(subgraphs, end, v);
	else
		add_inputs(start->subs, end, v);    
}

template<class V>
void Graph<V>::remove_edges(unsigned int u, unsigned int v) {
    assert(u < num_vertices());
    assert(v < num_vertices());
    outs[u].erase(std::remove(outs[u].begin(), outs[u].end(), v), outs[u].end());
    ins[v].erase(std::remove(ins[v].begin(), ins[v].end(), u), ins[v].end());
    
    // update inputs vector for all subgraphs
    subgraph* start = find_parent(u);
    subgraph* end = find_parent(v);
    
    if (start == 0)
    	remove_inputs(subgraphs, end, v);
	else
		remove_inputs(start->subs, end, v); 
}
#endif // GRAPH_HPP
