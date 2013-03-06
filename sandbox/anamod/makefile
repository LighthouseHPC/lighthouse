include ${PETSC_DIR}/conf/variables
include ${PETSC_DIR}/conf/rules

petscmetadataut: petscmetadataut.o  chkopts
	-${CLINKER} -o petscmetadataut petscmetadataut.o ${PETSC_MAT_LIB} 
	${RM} petscmetadataut.o

include ${PETSC_DIR}/conf/test
