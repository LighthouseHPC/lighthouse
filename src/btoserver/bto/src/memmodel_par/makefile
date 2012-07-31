all: aatx bicg bicg-u aatx-u gemver-u gemver

BTO_SRC = memmodel_par.c tree_par.c parallel_machines.c cost_par.c \
	build_machine.c ../../hardware_profiling/topology_parse.cpp
BTO_HRD = memmodel_par.h tree_par.h parallel_machines.h cost_par.h \
	build_machine.h ../../hardware_profiling/topology_parse.hpp
BTO_OBJ = tree_par.o parallel_machines.o cost_par.o memmodel_par.o \
	build_machine.o topology_parse.o 

bto: bto_mem_model.o
	@echo "Making memmodel..."

bto_mem_model.o : $(BTO_OBJ) $(BTO_SRC) $(BTO_HDR) 
	ld -r $(BTO_OBJ) -o bto_mem_model.o

build_machine.o: build_machine.h build_machine.c parallel_machines.h
	g++ -c -g build_machine.c -o build_machine.o

memmodel_par.o: memmodel_par.h memmodel_par.c tree_par.h
	gcc -c -g memmodel_par.c -o memmodel_par.o

parallel_machines.o: parallel_machines.c parallel_machines.h
	gcc -c -g parallel_machines.c -o parallel_machines.o

tree_par.o: tree_par.h tree_par.c
	gcc -c -g tree_par.c -o tree_par.o

cost_par.o: cost_par.h cost_par.c
	gcc -c -g cost_par.c -o cost_par.o

topology_parse.o: ../../hardware_profiling/topology_parse.cpp \
		../../hardware_profiling/topology_parse.hpp
	gcc -c -g ../../hardware_profiling/topology_parse.cpp -o topology_parse.o

bicg_driver.o: bicg.c memmodel_par.h tree_par.h parallel_machines.h cost_par.h build_machine.h
	g++ -c -g bicg.c -o bicg_driver.o

bicg: bicg_driver.o memmodel_par.o parallel_machines.o tree_par.o cost_par.o build_machine.o
	g++ memmodel_par.o parallel_machines.o tree_par.o cost_par.o ../../hardware_profiling/topology_parse.o build_machine.o bicg_driver.o -o bicg

bicg-u_driver.o: bicg-u.c memmodel_par.h tree_par.h parallel_machines.h cost_par.h build_machine.h
	g++ -c bicg-u.c -o bicg-u_driver.o

bicg-u: bicg-u_driver.o memmodel_par.o parallel_machines.o tree_par.o cost_par.o
	g++ memmodel_par.o parallel_machines.o tree_par.o cost_par.o ../../hardware_profiling/topology_parse.o build_machine.o bicg-u_driver.o -o bicg-u

aatx_driver.o: aatx.c memmodel_par.h tree_par.h parallel_machines.h cost_par.h build_machine.h
	g++ -c -g  aatx.c -o aatx_driver.o

aatx: aatx_driver.o memmodel_par.o parallel_machines.o tree_par.o cost_par.o
	g++ memmodel_par.o parallel_machines.o tree_par.o cost_par.o ../../hardware_profiling/topology_parse.o build_machine.o aatx_driver.o -o aatx

aatx-u_driver.o: aatx-u.c memmodel_par.h tree_par.h parallel_machines.h cost_par.h build_machine.h
	g++ -c aatx-u.c -o aatx-u_driver.o

aatx-u: aatx-u_driver.o memmodel_par.o parallel_machines.o tree_par.o cost_par.o build_machine.o
	g++ memmodel_par.o parallel_machines.o tree_par.o cost_par.o ../../hardware_profiling/topology_parse.o build_machine.o aatx-u_driver.o -o aatx-u

gemver-u_driver.o: gemver-u.c memmodel_par.h tree_par.h parallel_machines.h cost_par.h build_machine.h
	g++ -c gemver-u.c -o gemver-u_driver.o

gemver-u: gemver-u_driver.o memmodel_par.o parallel_machines.o tree_par.o cost_par.o build_machine.o
	g++ memmodel_par.o parallel_machines.o tree_par.o cost_par.o ../../hardware_profiling/topology_parse.o build_machine.o gemver-u_driver.o -o gemver-u

gemver_driver.o: gemver.c memmodel_par.h tree_par.h parallel_machines.h cost_par.h build_machine.h
	g++ -c gemver.c -o gemver_driver.o

gemver: gemver_driver.o memmodel_par.o parallel_machines.o tree_par.o cost_par.o build_machine.o
	g++ memmodel_par.o parallel_machines.o tree_par.o cost_par.o ../../hardware_profiling/topology_parse.o build_machine.o gemver_driver.o -o gemver

clean: 
	rm -f *.o aatx bicg bicg-u aatx-u gemver gemver-u
