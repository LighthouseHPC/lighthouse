%{

#include <iostream>
#include <fstream>
#include <map>
#include "boost/lexical_cast.hpp"
#include "syntax.hpp"
#include "syntax.tab.h"
#include "compile.hpp"
#include "boost/program_options.hpp"
#include <string>

int yylex();
extern int yylineno;
void yyerror(std::string s);
std::string out_file_name;
std::string input_file;
extern FILE *yyin; 
boost::program_options::variables_map vm;
%}

%union {
  expr* expression;
  stmt* statement;
  std::vector<stmt*>* program;
  std::string* variable;
  param* parameter;
  double number;
  std::map<string,type*>* parameter_list;
  type* value_type;
  storage store;
  std::string *orien;
}

%type <expression> expr
%type <statement> stmt
%type <program> prog
%type <parameter> param
%type <parameter_list> param_list
%type <value_type> type
%type <orien> orientation
%type <store> storage

%start input
%token NEG TIC IN INOUT OUT ROW COLUMN VECTOR MATRIX SCALAR GENERAL COORDINATE COMPRESSED
%token <variable> VAR
%token <number> NUM
%left '-' '+'
%left '*'
%nonassoc IN INOUT OUT
%nonassoc NEG     /* negation--unary minus */
%nonassoc TIC    /* transpose */

%%
input: VAR IN param_list INOUT param_list OUT param_list '{' prog '}' 
		{ compile(vm, out_file_name, *$1, *$3, *$5, *$7, *$9); }
	| VAR IN param_list INOUT param_list '{' prog '}'
		{ std::map<string,type*> *tmp = new std::map<string,type*>(); 
		compile(vm, out_file_name, *$1, *$3, *$5, *tmp, *$7); }
	| VAR INOUT param_list OUT param_list '{' prog '}'
		{ std::map<string,type*> *tmp = new std::map<string,type*>(); 
		compile(vm, out_file_name, *$1, *tmp, *$3, *$5, *$7); }
	| VAR IN param_list OUT param_list '{' prog '}'
		{ std::map<string,type*> *tmp = new std::map<string,type*>(); 
		compile(vm, out_file_name, *$1, *$3, *tmp, *$5, *$7); }
;
param: VAR ':' type { $$ = new param(*$1,$3); }
;
param_list: param { $$ = new std::map<string,type*>(); $$->insert(*$1); }
          | param_list ',' param { $$ = $1; $$->insert(*$3); }
; 
orientation: 
  ROW { $$ = new std::string("row"); }
| COLUMN { $$ = new std::string("column"); }
| { $$ = new std::string("column") }
;
storage:
  GENERAL { $$ = dense; }
| COMPRESSED { $$ = compressed; }
| COORDINATE { $$ = coordinate; }
| { $$ = dense; }
;
type: orientation MATRIX { $$ = new type($1, "matrix", dense); }
    | orientation VECTOR { $$ = new type($1, "vector", dense); }
    | SCALAR { $$ = new type(scalar); }
;
prog: { $$ = new std::vector<stmt*>(); }
    | prog stmt { $$ = $1; $$->push_back($2); }
;
stmt: VAR '=' expr { $$ = new stmt($1,$3); }
;
expr: NUM { $$ = new scalar_in($1); }
    | VAR { $$ = new variable($1); }
    | expr '+' expr { $$ = new operation(add, $1, $3); }
    | expr '-' expr { $$ = new operation(subtract, $1, $3); }
    | expr '*' expr { $$ = new operation(multiply, $1, $3); }
    | '-' expr %prec NEG { $$ = new operation(negate_op, $2); }
    | expr '\'' %prec TIC { $$ = new operation(trans, $1); }
    | '(' expr ')' { $$ = $2; }
;
%%
int main(int argc, char *argv[]) 
{
  namespace po = boost::program_options;
  
  po::options_description vis("Options");
  vis.add_options()
  		("help,h","this help message")

		("precision,a",po::value<std::string>()->default_value("double"),
  				"Set precision type for generated output [float|double]\n"
  				"  double is default")
		("empirical_off,e","Disable empirical testing.  Empirical testing\n"
				"is enabled by default")
		("correctness,c","Enable correctness testing.  Correctness testing\n"
				"is disabled by default.  Enabling requires a BLAS library\n"
				"to be present.  (Set path in top level make.inc. See \n"
				"documentation for further details")
		("model_off,m","Disable the analytic model when testing kernels. If "
				"set the compiler will empirically tests all of its "
				"optimization choices.  For even moderately "
				"complicated computations this can take a long time.")
		("threshold,t",po::value<double>()->default_value(0.01),
				"This parameter controls how much empirical testing is "
				 "performed. For example, the default of 0.01 says that "
				"empirical testing will be performed on any version "
				"that is predicted to be within %1 the performance of "
				"the best predicted version.  A value of 1 will "
				"rank all versions. A value of 0 will select only the "
				"best version.")
		("limit,l",po::value<int>()->default_value(-1),
				"Specifies a time limit in seconds for the empirical "
				"search.  Default is unlimited time.")
		("test_param,r",po::value<std::string>()->default_value("3000:3000:1"),
  				"Set parameters for empirical and correctness tests as\n"
  				"start:stop:step")
		("partition_off,p","Disable partitioning.  Enabled by default, generates\n"
				"parallel code that requires a Pthreads library.")
		("backend,b",po::value<std::string>()->default_value("ptr"),
				"select the code generation backend:\n"
				"  [ptr|noptr] \tptr is default\n"
				"  ptr: \tproduce c code using pointers\n"
				"  noptr: \tproduce c code using variable length arrays\n"
				"\nSelecting noptr requires partitioning to be disabled")

/*
  		("lower_dot,l","output initial lowering *.dot files (lower*.dot & rw*.dot)")
  		("opt_dot,d","output tmp__*.dot files showing optimizations (allows for version trace "
  				"creation with graph_all.py")


  		("mode,m",po::value<std::string>()->default_value("all"),
  				"select output mode: [all|best]\n"
  				"  all: \tproduce all possible variations of optimizations\n"
  				"  best: \tproduce only fully optimized version")

		*/
  		;
  
  po::options_description hidden("Hidden options");
  hidden.add_options()
  		("input-file", po::value<std::string>(), "input file")
		("run_all","allow compiler to run with model and empirical tests off")
  		;
  po::options_description cmdline_ops;
  cmdline_ops.add(vis).add(hidden);
  
  po::positional_options_description p;
  p.add("input-file", -1);
  
  po::store(po::command_line_parser(argc,argv).options(cmdline_ops).positional(p).run(), vm);
  po::notify(vm);
  
  if (vm.count("help") || !(vm.count("input-file"))) {
  	std::cout << "\nUSAGE: btoblas matlab_input_file.m [options]\n\n" << vis << std::endl;
  	return(1);
  }	
   
  if (!(vm["precision"].as<std::string>().compare("float") == 0 ||
  			vm["precision"].as<std::string>().compare("double") == 0)) {
  		std::cout << "ERROR:" << std::endl;
  		std::cout << "\t" << vm["precision"].as<std::string>() << " is not a valid "
  				  << "option for --precision (-p)" << std::endl;
  		std::cout << "\nUSAGE: btoblas matlab_input_file.m [options]\n\n" << vis
  				  << std::endl;
  		return(1);		
  }

  if (!(vm["backend"].as<std::string>().compare("ptr") == 0 ||
			vm["backend"].as<std::string>().compare("noptr") == 0)) {
		std::cout << "ERROR:" << std::endl;
        std::cout << "\t" << vm["backend"].as<std::string>() << " is not a valid option "
                  << "for --backend (-b)" << std::endl;
        std::cout << "\nUSAGE: btoblas matlab_input_file.m [options]\n\n" << vis << std::endl;
        return(1);

  }
  
  if ((vm["backend"].as<std::string>().compare("noptr") == 0) && !vm.count("partition_off")) {
	std::cout << "ERROR:" << std::endl;
    std::cout << "\t" << "Cannot enable partitioning with noptr backend selected.\n";
    std::cout << "\nUSAGE: btoblas matlab_input_file.m [options]\n\n" << vis << std::endl;
    return(1);
  }

	if (vm.count("model_off") && vm.count("empirical_off")  && !vm.count("run_all")) {
		std::cout << "ERROR:" << std::endl;
	    std::cout << "\t" << "Either model or empirical testing or both required.\n";
	    std::cout << "\nUSAGE: btoblas matlab_input_file.m [options]\n\n" << vis << std::endl;
	    return(1);
	}

  input_file = vm["input-file"].as<std::string>();
  
  out_file_name = input_file;
  size_t loc = out_file_name.rfind(".m");
  if (loc != string::npos)
  	out_file_name.replace(loc,2,"");
  else
  	std::cout << "WARNING:\n\tinput file \"" <<  input_file << "\" may not be a .m file\n";
  
  yyin = fopen(input_file.c_str(), "r");
  if (yyin == NULL) {
  	std::cout << "ERROR:\n\tunable to open \"" << input_file << "\"" << std::endl;;
  	std::cout << "\nUSAGE: btoblas matlab_input_file.m [options]\n\n" << vis << std::endl;
  	return(1);	
  } 
  
  yyparse(); 
  
  fclose(yyin);
  return 0;
} 
void yyerror (std::string s) /* Called by yyparse on error */ 
{ 
  std::cout << "line " << yylineno << ": " << s << std::endl;
} 
