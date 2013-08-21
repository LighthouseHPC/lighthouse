
include ${SLEPC_DIR}/conf/slepc_common

eigenvalue_solver: eigenvalue_solver.o  chkopts
	-${CLINKER} -o eigenvalue_solver eigenvalue_solver.o  ${SLEPC_LIB}
	${RM} eigenvalue_solver.o


