CPP = g++
C = gcc


OPTS = -O2#, this is causing a bus error in create_file
COPTS = -O2
FLAGS = $(OPTS)
M = #-m64

all: hardware_profile hardware_profile_par

reg: 
	gcc $(FLAGS) masearch.c
	./a.out d 1 1

force_build : ATL_walltime.o ATL_cputime.o

time.o : time.c force_build
	$(C) -c $(FLAGS) -I ./ time.c

ATL_walltime.o : ATL_walltime.c
	$(C) -c $(FLAGS) ATL_walltime.c

ATL_cputime.o : ATL_cputime.c
	$(C) -c $(FLAGS) ATL_cputime.c

xdmuladd : force_build muladd.c
	$(C) $(FLAGS) -c muladd.c
	$(C) $(FLAGS) -o xdmuladd muladd.o ATL_walltime.o ATL_cputime.o
	$(ATLRUN) ./xdmuladd

stream.o: stream.c stream.h
	$(C) -c $(OPTS) $(M) stream.c -o stream.o

stream2.o: stream2.c stream2.h
	$(C) $(OPTS) $(M) -std=c99 -c stream2.c -o stream2.o

eliminate.o: eliminate.cpp eliminate.hpp
	$(CPP) -c $(FLAGS) $(M) eliminate.cpp -o eliminate.o

mysecond.o: mysecond.c mysecond.h
	$(C) -c $(OPTS) $(M) stream.c -o stream.o

benchmark.o: benchmark.c benchmark.h
	$(C) -c $(COPTS) $(M) benchmark.c -o benchmark.o

cache_size.o: cache_size.cpp cache_size.hpp
	$(CPP) -c $(FLAGS) $(M) cache_size.cpp -o cache_size.o

bandwidth.o: bandwidth.cpp bandwidth.hpp stream.h
	$(CPP) -c $(FLAGS) $(M) bandwidth.cpp -o bandwidth.o

# Using the optimization flag -O2 causes a bus error in create_file.cpp
create_file.o: create_file.cpp create_file.hpp
	$(CPP) $(M) -c create_file.cpp -o create_file.o

reg_availible.o: reg_availible.cpp reg_availible.hpp
	$(CPP) -c $(FLAGS) $(M) reg_availible.cpp -o reg_availible.o

vector_reg_availible.o: vector_reg_availible.cpp vector_reg_availible.hpp
	$(CPP) -c $(FLAGS) $(M) vector_reg_availible.cpp -o vector_reg_availible.o

topology_parse.o: topology_parse.cpp topology_parse.hpp
	$(CPP) -c $(FLAGS) $(M) topology_parse.cpp -o topology_parse.o

hardware_profile.o: hardware_profile.cpp create_file.hpp bandwidth.hpp cache_size.hpp stream.h stream2.h reg_availible.hpp vector_reg_availible.hpp
	$(CPP) -c $(FLAGS) $(M) hardware_profile.cpp -o hardware_profile.o

hardware_profile: hardware_profile.o create_file.o bandwidth.o cache_size.o stream2.o benchmark.o stream.o reg_availible.o eliminate.o vector_reg_availible.o
	$(CPP) $(M) benchmark.o stream2.o create_file.o bandwidth.o cache_size.o stream.o eliminate.o reg_availible.o vector_reg_availible.o ../src/memmodel_an/machines.h hardware_profile.o -o hardware_profile

hardware_profile_par.o: hardware_profile_par.cpp create_file_par.hpp bandwidth.hpp benchmark.h stream.h reg_availible.hpp vector_reg_availible.hpp topology_parse.hpp create_file.hpp
 
hardware_profile_par: hardware_profile_par.o create_file_par.o bandwidth.o benchmark.o stream.o reg_availible.o vector_reg_availible.o topology_parse.o create_file.o
	$(CPP) $(M) benchmark.o create_file_par.o create_file.o bandwidth.o reg_availible.o vector_reg_availible.o stream.o ../src/memmodel_par/parallel_machines.o topology_parse.o hardware_profile_par.o -o hardware_profile_par

clean:
	rm -f *.o hardware_profile
