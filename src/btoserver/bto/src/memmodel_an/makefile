all: aatx bicg bicg-u aatx-u gemver-u gemver

BTO_SRC = memmodel_clean.c tree.c machines.c cost.c your_machine.c
BTO_HRD = memmodel_clean.h tree.h machines.h cost.h your_machine.h
BTO_OBJ = tree.o machines.o cost.o memmodel_clean.o machine.o

bto: bto_mem_model.o
	@echo "Making memmodel..."

bto_mem_model.o : $(BTO_OBJ) $(BTO_SRC) $(BTO_HDR) 
	ld -r $(BTO_OBJ) -o bto_mem_model.o

memmodel.o: memmodel_clean.h memmodel_clean.c tree.h
	gcc -c -g memmodel_clean.c -o memmodel.o

memmodel_clean.o: memmodel_clean.h memmodel_clean.c tree.h
	gcc -c -g memmodel_clean.c -o memmodel_clean.o

parallel_machines.o: parallel_machines.c parallel_machines.h
	gcc -c parallel_machines.c -o parallel_machines.o

machines.o: machines.h machines.c
	gcc -c -g machines.c -o machines.o	

machine.o:  your_machine.h your_machine.c
	gcc -c -g your_machine.c -o machine.o

tree.o: tree.h tree.c
	gcc -c -g tree.c -o tree.o

cost.o: cost.h cost.c
	gcc -c -g cost.c -o cost.o
	
bicg_driver.o: bicg.c memmodel_clean.h tree.h machines.h cost.h
	gcc -c -g bicg.c -o bicg_driver.o

bicg: bicg_driver.o memmodel.o machines.o tree.o cost.o
	gcc memmodel.o machines.o tree.o cost.o bicg_driver.o -o bicg

bicg-u_driver.o: bicg-u.c memmodel_clean.h tree.h machines.h cost.h
	gcc -c bicg-u.c -o bicg-u_driver.o

bicg-u: bicg-u_driver.o memmodel.o machines.o tree.o cost.o
	gcc memmodel.o machines.o tree.o bicg-u_driver.o cost.o -o bicg-u

aatx_driver.o: aatx.c memmodel_clean.h tree.h machines.h cost.h 
	gcc -c -g  aatx.c -o aatx_driver.o

aatx: aatx_driver.o memmodel.o machines.o tree.o cost.o
	gcc -g memmodel.o machines.o tree.o cost.o aatx_driver.o -o aatx 

aatx-u_driver.o: aatx-u.c memmodel_clean.h tree.h machines.h cost.h
	gcc -c aatx-u.c -o aatx-u_driver.o

aatx-u: aatx-u_driver.o memmodel.o machines.o tree.o cost.o
	gcc memmodel.o machines.o tree.o cost.o aatx-u_driver.o -o aatx-u

gemver-u_driver.o: gemver-u.c memmodel_clean.h tree.h machines.h cost.h
	gcc -c gemver-u.c -o gemver-u_driver.o

gemver-u: gemver-u_driver.o memmodel.o machines.o tree.o cost.o
	gcc memmodel.o machines.o tree.o gemver-u_driver.o cost.o -o gemver-u

gemver_driver.o: gemver.c memmodel_clean.h tree.h machines.h cost.h
	gcc -c gemver.c -o gemver_driver.o

gemver: gemver_driver.o memmodel.o machines.o tree.o cost.o
	gcc memmodel.o machines.o tree.o gemver_driver.o cost.o -o gemver

clean: 
	rm -f *.o aatx bicg bicg-u aatx-u gemver gemver-u
