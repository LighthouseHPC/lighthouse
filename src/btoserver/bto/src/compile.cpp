#include <fstream>
#include <iostream>
#include <stack>
#include <math.h>
#include "workSpace.hpp"
#include "build_graph.hpp"
#include "analyze_graph.hpp"
#include "optimizers.hpp"
#include "partition.hpp"
#include "enumerate.hpp"
#include "translate_to_code.hpp"
#include "compile.hpp"
#include "work.hpp"
#include "type_analysis.hpp"
#include "test_generator.hpp"
#include "boost/program_options.hpp"
#include "boost/timer.hpp"
#include <set>
#include <list>
#include "modelInfo.hpp"

#include "cost_estimate_common.hpp"
#include "memmodel_par/build_machine.h"
extern "C" {
#include "memmodel_par/parallel_machines.h"
}

// output graphs files (this is not dot files)
#define DUMP_GRAPHS

// output dot file for each version
#define DUMP_DOT_FILES

// output vid : history string for each version.
#define DUMP_VERSION_STRINGS

// print any available data for model and empirical testing
#define DUMP_DATA
// dump this data to file routine_name.csv when true
//or stdout when false
#define DUMP_DATA_TO_FILE true

// when MODEL_TIME is define, the time spent in the memory model will
// be displayed.  Requires USE_COST or USE_COST_SYM to be defined.
//#define MODEL_TIME

//set precision type
std::string precision_type;

int subgraphUniqueID;

void getDependencies(graph &g) {
	// find dependencies such as
	// row0 < row1 < scl >>
	// the size of row1 must be <= the size of row0
	
	// in this process, check for incorrect 
	// dim, step, base_rowsm base_cols, lead_dim
	
	vertex v;
	for (v = 0; v < g.num_vertices(); ++v) {
		if (g.info(v).op == deleted)
			continue;
		
		type *t = &g.info(v).t;
		
		list<string> rows, cols;
		rows.clear();
		cols.clear();
		
		if (t->k == scalar) {
			continue;
			if (t->dim.dim.compare("1") != 0
				|| t->dim.step.compare("1") != 0
				|| t->dim.base_rows.compare("1") != 0
				|| t->dim.base_cols.compare("1") != 0
				|| t->dim.lead_dim.compare("1") != 0) {
				std::cout << "Scalar error\n";
				std::cout << "\t" << t->dim.dim << "," << t->dim.step
				<< "," << t->dim.base_rows << "," 
				<< t->dim.base_cols << "," << t->dim.lead_dim
				<< "\n";
			}
		}
		else {
			if (t->k == row)
				rows.push_front(t->dim.dim);
			else
				cols.push_front(t->dim.dim);
		}
		
		string base_rows = t->dim.base_rows;
		string base_cols = t->dim.base_cols;
		string lead_dim = t->dim.lead_dim;
		t = t->t;
		
		while (t->k != scalar) {
			if (t->k == row)
				rows.push_front(t->dim.dim);
			else
				cols.push_front(t->dim.dim);
			
			if (base_rows.compare(t->dim.base_rows) != 0
				|| base_cols.compare(t->dim.base_cols) != 0
				|| lead_dim.compare(t->dim.lead_dim) != 0) {
				std::cout << "ERROR " << v << "\n";
				std::cout << prnt_detail(&(g.info(v).t)) << "\n";
			}
			t = t->t;
		}
		
		// 2 questions to answer
		// 1) is a matrix partitioned in more than one dimension
		//			rows.size() && cols.size() > 1
		// 2) is any structure partitioned more than once in a given
		//      dimension.
		//			rows.size() || cols.size() > 2
		
		// this information needs to be based on operations
		// consider gemm partitoned 3 ways.  Looking at any one
		// of the data structures would be misleading.
		// maybe no... there is a link in that some dimension
		// will show up in two pieces of data that are partitioned
		// in more than one dimension without common buddies
		// (m,k) and (k,n)
		// though is there a case where m and n could be forced to be
		// the same.
		// C = A * A' damnit
		/*
		 std::cout << v << "\n\trows: ";
		 list<string>::iterator i;
		 for (i = rows.begin(); i != rows.end(); ++i)
		 std::cout << *i << "\t";
		 std::cout << "\n\tcols: ";
		 for (i = cols.begin(); i != cols.end(); ++i)
		 std::cout << *i << "\t";
		 std::cout << "\n\n";
		 */
	}
}


void typeToQuery(string id, vertex v, type *t, std::ofstream &fout, 
				 op_code op) {	
	
	fout << v << ";" << id << ";" << op << "&&"; 
	
	while (t) {
		
		fout << t->height << ";";
		
		if (t->t) {
			int part = t->t->whichPartition(t->dim.step, t->k);
			if (part > 0)
				fout << part << ";";
			else 
				fout << ";";
		}
		else 
			fout << ";";
		
		if (t->k == row)
			fout << "row";
		else if (t->k == column)
			fout << "col";
		else if (t->k == scalar)
			fout << "scl";
		fout << ";";
		
		fout << t->dim.dim;
		
		t = t->t;
		if (t)
			fout << "::";
	}
	fout << "\n";
}

void graphToQuery_r(graph &g, std::ofstream &fout, vertex v, 
					vector<string> &connect) {
	
	string toMe;
	if (connect.size() > 0) {
		toMe = string(connect.back());
		connect.erase(connect.end());
	}
	else { 
		toMe = "";
	}
	
	string withMe = toMe + boost::lexical_cast<string>(v) + ",";
	
	if (g.adj(v).size() == 0) {
		//if (g.find_parent(v) == 0 && g.info(v).op != input) {
		connect.push_back(withMe);
		withMe = boost::lexical_cast<string>(v) + ",";
	}
	
	for (int i = 0; i < g.adj(v).size(); ++i) {
		vertex l = g.adj(v)[i];
		
		connect.push_back(withMe);
		graphToQuery_r(g,fout,l,connect);
	}
}

void graphToFusionQuery_r(subgraph *sg, std::ofstream &fout, vector<string> &connect) {
	string toMe;
	if (connect.size() > 0) {
		toMe = string(connect.back());
		connect.erase(connect.end());
	}
	else { 
		toMe = "";
	}
	
	string withMe = toMe + boost::lexical_cast<string>(sg->uid) + ",";
	
	if (sg->subs.size() == 0) {
		connect.push_back(withMe);
	}
	
	fout << sg->uid << "::";
	for (unsigned int i = 0; i < sg->vertices.size(); ++i)
		fout << sg->vertices[i] << ",";
	
	fout << "\n";
	
	for (unsigned int i = 0; i < sg->subs.size(); ++i) {
		connect.push_back(withMe);
		graphToFusionQuery_r(sg->subs[i], fout, connect);
	}
}

void graphToQuery(graph &g, string fileName,unsigned int vid, unsigned int threadDepth) {
	std::ofstream fout((fileName + "__q" + boost::lexical_cast<string>(vid) 
						+ ".bto").c_str());
	
	fout << "## ops\n";
	for (unsigned int i = 0; i < 22; ++i)
		fout << i << "," << op_to_name((op_code)i) << ";";
	fout << "\n";
	
	fout << "## thread depth\n";
	fout << threadDepth << "\n";
	
	fout << "## vertices\n";
	std::set<string> connections; 
	for (vertex v = 0; v < g.num_vertices(); ++v) {
		if (g.info(v).op == input) {
			vector<string> connect;
			graphToQuery_r(g,fout,v,connect);
			
			for (unsigned int i = 0; i < connect.size(); ++i) {
				connections.insert(connect[i]);
			}
		}
		if (g.info(v).op != deleted) {
			string lbl = g.info(v).label;
			if (lbl.compare("") == 0)
				lbl = "t"+boost::lexical_cast<string>(v);
			
			typeToQuery(lbl, v, &g.info(v).t, fout, g.info(v).op);
		}
	}
	
	fout << "## vertex connections\n";
	std::set<string>::iterator i;
	for (i = connections.begin(); i != connections.end(); ++i) {
		fout << *i << "\n";
	}
	
	fout << "##\n\n## Loops\n";
	fout << "0::";
	for (vertex v = 0; v < g.num_vertices(); ++v) {
		if (g.info(v).op != deleted && g.find_parent(v) == 0)
			fout << v << ",";
	}
	fout << "\n";
	
	connections.clear();
	for (unsigned int i = 0; i < g.subgraphs.size(); ++i) {
		vector<string> connect;
		graphToFusionQuery_r(g.subgraphs[i],fout, connect);
		
		for (unsigned int i = 0; i < connect.size(); ++i) {
			connections.insert(connect[i]);
		}
	}
	
	fout << "## loop connections\n";
	for (i = connections.begin(); i != connections.end(); ++i) {
		fout << *i << "\n";
	}
	fout << "##\n\n";
	fout.close();
}


void compile(boost::program_options::variables_map vm,
			 string out_file_name,
			 string routine_name,
			 map<string,type*>& inputs,
			 map<string,type*>& inouts,
			 map<string,type*>& outputs,
			 vector<stmt*> const& prog)
{
	bool noptr;
	if (vm["backend"].as<std::string>().compare("noptr") == 0) {
		noptr = true;
	} else {
		noptr = false;
	}
	
	bool partitionSet = true;
	if (vm.count("partition_off")) {
		partitionSet = false;
	}
	
	// determine location of bto (top level directory)
	char* glblPath = getcwd(NULL,0);	
	char *loc = strstr(glblPath,"bto");
	if (loc == NULL) {
		printf("Unable to determine path to bto\n");
		if (glblPath)
			free(glblPath);
		glblPath = (char*)malloc(sizeof(char)*200);
		while (1) {
			printf("Please enter the path or 'quit' to exit\n");
			if (scanf("%s",glblPath) == 0)
				continue;
			if (strstr(glblPath,"quit") == glblPath)
				return;
			
			loc = strstr(glblPath,"bto");
			if (loc != NULL)
				break;
		}
		while (getchar() != '\n');
	}
	
	int len = loc - glblPath;
	char *topPath = (char*)malloc(sizeof(char)*(len+5));
	strncpy(topPath,glblPath,len+3);
	topPath[len+3] = '/';
	topPath[len+4] = '\0';
	
	string pathToTop = string(topPath);
	free(glblPath);
	free(topPath);
	
	// set up temporary workspace
	string tmpPath, fileName, path;
	if (setUpWorkSpace(out_file_name, fileName, path, tmpPath, pathToTop)) {
		std::cout << "Failed to set up temporary directory for unique implementations\n";
		return;
	}
	
	// set all work to be performed in temporary work directory
	out_file_name = tmpPath + fileName;
	
	
	///////////// COST MODELS ///////////////////
	std::list<versionData*> orderedVersions;
	string testParam = vm["test_param"].as<std::string>();
	string start, stop, step;
	
	string::size_type pos = testParam.find_first_of(":");
	if (pos != string::npos)
		start = testParam.substr(0, pos);
	string::size_type last = pos+1;
	pos = testParam.find_first_of(":",last);
	if (pos != string::npos)
		stop = testParam.substr(last, pos-last);
	step = testParam.substr(pos+1,string::npos);
	
	modelMsg msg(boost::lexical_cast<int>(start),
				 boost::lexical_cast<int>(stop),
				 boost::lexical_cast<int>(step));
	msg.pathToTop = pathToTop;
	// build_machine 0 -> parallel, 1 -> serial
	msg.parallelArch = build_machine((char*)(pathToTop.c_str()),0);
	if (msg.parallelArch == NULL) {
		std:: cout << "Error attempting to get cost with parallel analytic model\n";
		return;
	}
	msg.serialArch = build_machine((char*)(pathToTop.c_str()),1);
	if (msg.serialArch == NULL) {
		std:: cout << "Error attempting to get cost with parallel analytic model\n";
		return;
	}
	////////////////////////////////////////////
	
	// Unique Identifier for each subgraph created
	subgraphUniqueID = 1;
	
	precision_type = vm["precision"].as<std::string>();
	
#ifdef MODEL_TIME
	// time spent in memmodel routines
	boost::timer memmodelTimer;
	double memmodelTotal = 0.0;
	
#define CLEAR_MODEL_TIME memmodelTimer.restart()
#define GET_MODEL_TIME memmodelTotal += memmodelTimer.elapsed()
#else
#define CLEAR_MODEL_TIME
#define GET_MODEL_TIME
#endif
	
	
	double threshold;
	if (vm.count("model_off")) {
		std::cout << "\nAnalytic model is disabled\n\n";
	}
	else {
		threshold = vm["threshold"].as<double>();
	}
	
	std::vector<std::pair<int, double> >::iterator itr;
    
	graph g;
	
	std::vector<algo> algos;
	//std::cerr << "finished parsing" << std::endl;
	program2graph(prog, inputs, inouts, outputs, g);  
	//std::cerr << "graph created" << std::endl;
	
	//use yices to compute types externally
	//#define TYPES
#ifdef TYPES
    std::ofstream out("lower0.dot");
    print_graph(out, g); 
	out.close()
    generate_constraints(g);
#endif
	compute_types(g);
#ifdef TYPES
    std::ofstream out("lower1.dot");
    print_graph(out, g);
	out.close();
    exit(0);
#endif
    
	update_sizes(g);
	update_dim_info(g);
	
	init_algos(algos);
		
	assign_algorithms(algos, g);
	//std::cerr << "algorithms assigned" << std::endl;
	
	assign_work(algos, g);
	//std::cerr << "work assigned" << std::endl;
	
	rewrite_fun rewriters[] =
    {	flip_transpose,
		flip_transpose_stride,
		remove_scalar_transpose,
		merge_tmp_output,
		remove_intermediate_temporary,
		merge_gets,
		merge_sumto_store,
		move_temporary
    };
	
	optim_fun optimizers[] =
    {	fuse_loops_new,			//0
		merge_scalars_new,		//1
		pipeline_new			//2
    };
	/*
	 std::string opt_names[] = {
	 "fuse_loops", 
	 "merge_scalars",
	 "pipeline"
	 };
	 */
	optim_fun_chk checkers[] =
    {	check_fuse_loops_new, 
		check_merge_scalars_new,
		check_pipeline_new,
    };
	
	rewrite_fun partition[] = {
		partition_add,			//0
		part_mult_left_result,	//1
		part_mult_right_result,	//2
		part_mult_left_right, 	//3
		part_mult_scl			//4
	};
	rewrite_fun part_checkers[] = {
		check_partition_add,
		check_part_mult_left_result,
		check_part_mult_right_result,
		check_part_mult_left_right,
		check_part_mult_scl
	};
	
	vector<rewrite_fun> rewrites(rewriters, rewriters + 
								 sizeof(rewriters) / sizeof(rewrite_fun));
	
	vector<optim_fun> optimizations(optimizers, optimizers + 
									sizeof(optimizers) / sizeof(optim_fun));
	vector<optim_fun_chk> checks(checkers, checkers + 
								 sizeof(checkers) / sizeof(optim_fun_chk));
	vector<rewrite_fun> part_checks(part_checkers, part_checkers + 
									sizeof(part_checkers)/sizeof(rewrite_fun));
	vector<rewrite_fun> partitioners(partition, partition +
									 sizeof(partition) / sizeof(rewrite_fun));
	
	//std::cerr << "about to start lowering and optimizine" << std::endl;
	
	std::stack<work_item> s;
	string history = "";
	
#ifdef DUMP_DOT_FILES
  	std::ofstream out("lower1.dot");
	print_graph(out, g);
	out.close();
#endif
	
#ifdef DUMP_GRAPHS
	graphToQuery(g,out_file_name,0,0);
#endif
	
	if (vm.count("correctness")) {
		createCorrectnessTest(g, routine_name, out_file_name, inputs, outputs,msg,noptr);
	}
	
	int baseDepth;
	if (partitionSet) {
		// determine depth before partitioning
		graph *ng = new graph(g);
		initial_lower(*ng, algos, rewrites);
		baseDepth = 0;
		check_depth(1,baseDepth, ng->subgraphs);
		ng->del();
		
		// min_depth can be used to force partitions.  For example
		// if only partitioned code is to be generated min_depth
		// should be set to baseDepth+1
		int min_depth = 0;
		// max_depth puts an upper limit on the amount of partitioning.
		// The amount of partitioning is linked to the type of operation,
		// for example matrix-matrix can be partitioned in 3 unique ways,
		// matrix-vector can be partitioned in 2 unique ways, and vector-vector
		// or scalar-vector can be partitioned in 1 unique way.  To allow 2 levels
		// of partitioning use baseDepth+2.
		// TODO: need to figure out what type of operation we are partitioning
		// so this can be automated.
		int max_depth = baseDepth + 1;
		find_partitioning(g, s, part_checks, partitioners, algos, rewrites, 
						  false, min_depth, max_depth);  
		//std::cout << "partitioning done\n";
	}
	
	initial_lower(g, algos, rewrites);
	//std::cout << "finished lowering and rewriting" << std::endl;
	
	
	
	int threadDepth = 0;
	
	/////////// NEW STACK
	// this is a new graph so I can blindly cleanup
	// if the original graph is newed to start with
	// this can change
	graph *newG = new graph(g);
  	work_item w(newG, 0,0, "",1);
  	s.push(w);
	
  	int vid = 1;		// unique version id
	
  	while (! s.empty()) {
		
  		work_item current = s.top();
		s.pop();
		
		// Work items either represent code to be generated or graphs
		// that have not been completely evaluated for loop merging combintations.
		// If this work item represents a graph that has been completely 
		// evaluated, generate code, otherwise create work items for each
		// new optimization
		if (!enumerate_loop_merges(s, current, checks, optimizations, rewrites,
								   algos)) {
			// all nesting levels have been evaluated
			
			// find dependencies and check types
			getDependencies(*current.g);
			
			
			// generate code
			
			std::ofstream fout((out_file_name + "__" + boost::lexical_cast<string>(vid) 
								+ ".c").c_str());
			if (partitionSet) {
				// determine the current depth
				// if curDepth > baseDepth
				//		we must have added partitions for parallel
				//		go parallel
				// else
				//		go to C
				int curDepth = 0;
				check_depth(1,curDepth, (current.g)->subgraphs);
				bool par = false;
				bool potentialPar = false;
				
				if (curDepth > baseDepth) {
					par = true;
					potentialPar = true;
					threadDepth = curDepth - baseDepth;
					switch (threadDepth) {
						case 1:
							// task level parallelism
							//if (current.g->subgraphs.size() != 1)
							//	par = false;
							// parallel reductions
							//else if (current.g->subgraphs[0]->summations.size() != 0)
							//	par = false;
							break;
						case 2:
							// task level parallelism
							if (current.g->subgraphs.size() != 1 
								|| current.g->subgraphs[0]->subs.size() != 1)
								par = false;
							// parallel reductions
							else if (current.g->subgraphs[0]->summations.size() != 0
									 || current.g->subgraphs[0]->subs[0]->summations.size() != 0)
								par = false;
							// partition same dimension twice (this needs a better partition
							// size generator)
							else if (current.g->subgraphs[0]->subs[0]->iterations.find("$$") == 0)
								par = false;
							break;
						case 3:
							// task level parallelism
							if (current.g->subgraphs.size() != 1 
								|| current.g->subgraphs[0]->subs.size() != 1
								|| current.g->subgraphs[0]->subs[0]->subs.size() != 1)
								par = false;
							// parallel reductions
							else if (current.g->subgraphs[0]->summations.size() != 0
									 || current.g->subgraphs[0]->subs[0]->summations.size() != 0
									 || current.g->subgraphs[0]->subs[0]->subs[0]->summations.size() != 0)
								par = false;
							// partition same dimension twice (this needs a better partition
							// size generator)
							else if (current.g->subgraphs[0]->subs[0]->iterations.find("$$") == 0
									 || current.g->subgraphs[0]->subs[0]->subs[0]->iterations.find("$$")
									 == 0)
								par = false;
							break;
						default:
							std::cout << "ERROR: compile.cpp; compile(); unexpected"
							<< " graph in parallel code gen\n";
					}
				}
				//if (curDepth > baseDepth) {
				//	par = true;
				//	threadDepth = 1;
				//}
#ifdef DUMP_GRAPHS
				graphToQuery(*current.g,out_file_name,vid,threadDepth);
#endif
				if (par) {
					translate_to_pthreads(fout, routine_name, inputs, outputs, *current.g,
										  threadDepth,msg.parallelArch->threads);
				}
				else if (noptr) {
					translate_to_noPtr(fout, routine_name, inputs, outputs, *current.g);
				} else {
					translate_to_intrin(fout, routine_name, inputs, outputs, *current.g);
				}
				
#ifdef DUMP_VERSION_STRINGS
				std::cout << vid << " : " << current.history;
				if (par)
					std::cout << "\t<- parallel\n";
				else if (potentialPar)
					std::cout << "\t<- potential parallel\n";
				else
					std::cout << "\n";
#endif
			} else {// PARTITION
#ifdef DUMP_VERSION_STRINGS
				std::cout << vid << " : " << current.history << "\n";
#endif
				
#ifdef DUMP_GRAPHS
				graphToQuery(*current.g,out_file_name,vid,0);
#endif
				if (noptr) {
					translate_to_noPtr(fout, routine_name, inputs, outputs, *current.g);
				} else {
					translate_to_intrin(fout, routine_name, inputs, outputs, *current.g);
				}
				
			}  // PARTITION
			fout.close();
			
#ifdef DUMP_DOT_FILES
			// print graph 
			std::ofstream out(string("lower" + boost::lexical_cast<string>(vid+100) 
									 + ".dot").c_str());
			print_graph(out, *current.g); 
			out.close();
#endif
			
			
			
			//////////////////// models ///////////////////
			// analyticSerial, tempCount, analyticParallel, symbolic, noModel			
			vector<model_typ> models;
			models.clear();
			// the model we want to rank with should go first in this list
			//models.push_back(noModel);
			models.push_back(analyticParallel);
			//models.push_back(analyticSerial);
			//models.push_back(tempCount);
			
			versionData *verData = new versionData(vid);
			
			
			
			if (! vm.count("model_off")) {
				// model is on
				CLEAR_MODEL_TIME;
				msg.threadDepth = threadDepth;
				modelVersion(orderedVersions, verData, models, *current.g, msg,
							 threshold,vid, routine_name);
				GET_MODEL_TIME;
			}
			else {
				// model is off
				orderedVersions.push_back(verData);				
			}
			/////////////////// end models /////////////////////
			
			// clean up work item
			current.del();
			
			vid++;
			
			continue;
		}
		
		// clean up work item
		current.del();
  	}
	
 	
	/////////////// Post Compilation Testing and Reporting ////////////////
	std::cout << "\n\nTotal Number of Versions Generated: " << vid-1 << "\n\n";
	
#ifdef MODEL_TIME
	// display time spent in memory model
	std::cout << "Time spent in memory model: " << memmodelTotal << " seconds\n\n";
#endif // MODEL_TIME
	
	int bestVersion;
	
	bool empiricalTest;
	if (!vm.count("empirical_off")) {
		empiricalTest = true;
	} else {
		empiricalTest = false;
	}
	
	// perrform empirical testing if enabled
	if (empiricalTest) {
		// Handle Any Empirical Testing
		
		// if the list has a single version
		// true: test the one
		// false: do not test the one
		bool testOne = true;
		
		if (!testOne) {
			std::cout << "A single version has been sent to empirical testing, skipping\n\n";
			bestVersion = (*orderedVersions.begin())->vid;
		}
		else {
			std::cout << "Empirically testing " << orderedVersions.size() << " versions.\n\n";
			createEmpiricalTest(g, routine_name, out_file_name, inputs, outputs, noptr);
			bestVersion = runEmpiricalTest(tmpPath, fileName, orderedVersions, 
										   vm["limit"].as<int>(), msg);
		}
	}
	else {
		std::cout << "Warning: Empirical testing has been disabled\n";
		if (orderedVersions.size() > 0)
			bestVersion = (*orderedVersions.begin())->vid;
	}
	
	// One way or another we have selected a version we are calling the best
	// or we failed
	if (bestVersion < 0) {
		std::cout << "All versions failed to generate or compile\n";
		return;
	}
	else {
		std::cout << "\n----------------------------------\n";
		std::cout << "Best Version: " << fileName + "__" << bestVersion << ".c\n";
	}
	
	// copy best version to same directory as input .m file
	// and report location
	handleBestVersion(fileName,path,tmpPath,bestVersion);
	
#ifdef DUMP_DATA
	// print data
	printData(orderedVersions,fileName+".csv",DUMP_DATA_TO_FILE);
#endif
	
	std::cout << "\n\n";
	
	if (vm.count("correctness")) {
		runCorrectnessTest(tmpPath, fileName, orderedVersions);
	}
	
	
	
	// clean up
	cleanVersionList(orderedVersions);
	delete_machineP(msg.parallelArch);
	delete_machineP(msg.serialArch);
}
