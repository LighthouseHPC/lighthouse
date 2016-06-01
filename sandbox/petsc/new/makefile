CFLAGS	         = -g -O3 ${PETSC_CC_INCLUDES}
CLINKER          = ${SL_LINKER}
EXAMPLESC        = solver.c parallel.c parallel-bgq.c sequential.c convert.c mm2petsc.c properties.c
CLEANFILES       = solver parallel parallel-bgq sequential convert mm2petsc properties
NP               = 1

#include ${PETSC_DIR}/${PETSC_ARCH}/conf/petscvariables
#include ${PETSC_DIR}/${PETSC_ARCH}/conf/petscrules
include ${PETSC_DIR}/lib/petsc/conf/variables
include ${PETSC_DIR}/lib/petsc/conf/rules


solver: solver.o  
	-${CLINKER} -o solver solver.o  ${PETSC_KSP_LIB}
	${RM} solver.o

sequential: sequential.o  
	-${CLINKER} -o sequential sequential.o  ${PETSC_KSP_LIB}
	${RM} sequential.o

parallel: parallel.o  
	-${SL_LINKER} -o parallel parallel.o  ${PETSC_KSP_LIB}
	${RM} parallel.o

parallel-bgq: parallel-bgq.o  
	-${CLINKER} -o parallel-bgq parallel-bgq.o  ${PETSC_KSP_LIB} 
	${RM} parallel-bgq.o

parallel-cram: parallel-bgq.o  
	-${CLINKER} -o parallel-cram parallel-bgq.o ${PETSC_KSP_CRAM_LIB}

convert: convert.o  
	-${CLINKER} -o convert convert.o  ${PETSC_MAT_LIB}
	${RM} convert.o

mm2petsc: mm2petsc.o  
	-${CLINKER} -o mm2petsc mm2petsc.o  ${PETSC_MAT_LIB}
	${RM} mm2petsc.o

properties: properties.o  
	-${CLINKER} -o properties properties.o  ${PETSC_MAT_LIB}
	${RM} properties.o

cpi-cram: cpi.o  
	-${CLINKER} -o cpi-cram cpi.o  ${HOME}/cram/lib/libcram.a

