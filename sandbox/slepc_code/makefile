
include ${SLEPC_DIR}/conf/slepc_common

eigenvalue_solver: eigenvalue_solver.o  chkopts
	-${CLINKER} -o eigenvalue_solver eigenvalue_solver.o  ${SLEPC_LIB}
	${RM} eigenvalue_solver.o

eigenvalue_parallel: eigenvalue_parallel.o  chkopts
	-${CLINKER} -o eigenvalue_parallel eigenvalue_parallel.o  ${SLEPC_LIB}
	${RM} eigenvalue_parallel.o

matrix_load: matrix_load.o  chkopts
	-${CLINKER} -o matrix_load matrix_load.o  ${PETSC_MAT_LIB}
	${RM} matrix_load.o


