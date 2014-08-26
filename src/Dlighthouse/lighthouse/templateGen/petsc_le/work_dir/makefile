include ${PETSC_DIR}/conf/variables
include ${PETSC_DIR}/conf/rules

linear_solver: linear_solver.o  chkopts
	-${CLINKER} -o linear_solver linear_solver.o  ${PETSC_KSP_LIB}
	${RM} linear_solver.o

include ${PETSC_DIR}/conf/test
