#ifndef COMPILE_HPP
#define COMPILE_HPP

#include "boost/program_options.hpp"

void compile(boost::program_options::variables_map vm,
		 string out_file_name, 
	     string routine_name,
	     map<string,type*>& inputs, 
	     map<string,type*>& inouts,
	     map<string,type*>& outputs,
	     vector<stmt*> const& prog);

#endif //COMPILE_HPP
