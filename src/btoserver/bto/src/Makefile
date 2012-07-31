include ../make.inc

INCLUDES = -I./

LIBS = 

OPTS = -O3 

FLAGS = $(INCLUDES) $(OPTS) -g

MEM_MODELS = bto_mem_model_AS.o bto_mem_model_AP.o

HEADERS = syntax.hpp graph.hpp translate_to_code.hpp \
	analyze_graph.hpp build_graph.hpp compile.hpp \
	optimizers.hpp partition.hpp type_analysis.hpp work.hpp \
	cost_estimate_common.hpp md5.h md5wrapper.h enumerate.hpp \
	test_generator.hpp workSpace.hpp modelInfo.hpp 
	

CPPSOURCES = syntax.cpp build_graph.cpp analyze_graph.cpp \
	compile.cpp translate_utils.cpp optimizers.cpp \
	translate_to_code.cpp partition.cpp type_analysis.cpp \
	work.cpp md5wrapper.cpp md5.cpp \
	enumerate.cpp test_generator.cpp workSpace.cpp \
	modelInfo.cpp cost_estimate_tmp.cpp cost_estimate.cpp \
	cost_estimate_par.cpp

C_BACKEND_SOURCES = translate_to_c.cpp
INTRIN_BACKEND_SOURCES = translate_to_noPtr.cpp translate_to_intrin.cpp translate_to_pthreads.cpp
CUDA_BACKEND_SOURCES = translate_to_cuda.cpp

GEN_C = lex.yy.c syntax.tab.c 

.SUFFIXES: .o .c .cpp


.c.o: $(HEADERS) 
	$(CPP) $(FLAGS) -c $*.c

.cpp.o: $(HEADERS) 
	$(CPP) $(FLAGS) -c $*.cpp

COBJECTS = $(GEN_C:.c=.o)
CPPOBJECTS = $(CPPSOURCES:.cpp=.o)
OBJECTS = $(COBJECTS) $(CPPOBJECTS)
C_BACKEND = $(C_BACKEND_SOURCES:.cpp=.o)
CUDA_BACKEND = $(CUDA_BACKEND_SOURCES:.cpp=.o)
INTRIN_BACKEND = $(INTRIN_BACKEND_SOURCES:.cpp=.o)

default: btoblas 
all: btoblas


frontend: syntax.y syntax.l
	bison -d syntax.y
	flex syntax.l

boost_program_options.o: 
	cd boost/src/program_options ; $(MAKE)
	cp -f boost/src/program_options/boost_program_options.o boost_program_options.o	
	
btoblas: $(OBJECTS) $(C_BACKEND) $(CUDA_BACKEND) $(INTRIN_BACKEND) boost_program_options.o $(MEM_MODELS)
	@echo "Making compiler..."
	$(CPP) $(FLAGS) $(LIBS) $^ -o $@
	rm bto_mem_model*.o
	
btonm: 	$(OBJECTS) $(C_BACKEND) $(CUDA_BACKEND) $(INTRIN_BACKEND)
		$(CPP) $(FLAGS) $(LIBS) $^ -o $@
	
bto_mem_model_AS.o :
	cd ./memmodel_an ; $(MAKE) bto
	cp -f ./memmodel_an/bto_mem_model.o bto_mem_model_AS.o
	
bto_mem_model_AP.o :
	cd ./memmodel_par ; $(MAKE) bto
	cp -f ./memmodel_par/bto_mem_model.o bto_mem_model_AP.o

clean:
	rm -rf btoblas btonm *.o
	cd ./memmodel_an ; $(MAKE) clean
	cd ./memmodel_par ; $(MAKE) clean

clean_frontend:
	rm -rf $(GEN_C) syntax.tab.h syntax.tab.o lex.yy.o

clean_all:
	$(MAKE) clean
	$(MAKE) clean_frontend
	cd boost/src/program_options ; $(MAKE) clean
