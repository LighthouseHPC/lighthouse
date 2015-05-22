CFLAGS	         = -g -O3
EXAMPLESC        = solver.c parallel.c parallel-bgq.c sequential.c convert.c mm2petsc.c properties.c
CLEANFILES       = solver parallel parallel-bgq sequential convert mm2petsc properties
NP               = 1

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

parallel-bgq: parallel-bgq.o  chkopts
	-${CLINKER} -o parallel-bgq parallel-bgq.o  ${PETSC_KSP_LIB} ${HOME}/cram/lib/libcram.a
	${RM} parallel-bgq.o

convert: convert.o  chkopts
	-${CLINKER} -o convert convert.o  ${PETSC_MAT_LIB}
	${RM} convert.o

mm2petsc: mm2petsc.o  chkopts
	-${CLINKER} -o mm2petsc mm2petsc.o  ${PETSC_MAT_LIB}
	${RM} mm2petsc.o

properties: properties.o  chkopts
	-${CLINKER} -o properties properties.o  ${PETSC_MAT_LIB}
	${RM} properties.o

