include ${PETSC_DIR}/conf/variables
include ${PETSC_DIR}/conf/rules

solver: solver.o  chkopts
	-${CLINKER} -o solver solver.o  ${PETSC_KSP_LIB}
	${RM} solver.o

sequential: sequential.o  chkopts
	-${CLINKER} -o sequential sequential.o  ${PETSC_KSP_LIB}
	${RM} sequential.o

parallel: parallel.o  chkopts
	-${CLINKER} -o parallel parallel.o  ${PETSC_KSP_LIB}
	${RM} parallel.o

convert: convert.o  chkopts
	-${CLINKER} -o convert convert.o  ${PETSC_MAT_LIB}
	${RM} convert.o

properties: properties.o  chkopts
	-${CLINKER} -o properties properties.o  ${PETSC_MAT_LIB}
	${RM} properties.o
