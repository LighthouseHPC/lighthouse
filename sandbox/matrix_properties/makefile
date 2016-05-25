# On arya, do "module load petsc/git" before building with "make properties"
CFLAGS	         = -g -O3 ${PETSC_CC_INCLUDES}
CLINKER          = ${SL_LINKER}
EXAMPLESC        = mm2petsc.c properties.c
CLEANFILES       = mm2petsc properties
NP               = 1

include ${PETSC_DIR}/lib/petsc/conf/variables
include ${PETSC_DIR}/lib/petsc/conf/rules

all: properties

mm2petsc: mm2petsc.o  
	-${CLINKER} -o mm2petsc mm2petsc.o  ${PETSC_MAT_LIB}
	${RM} mm2petsc.o

properties: properties.o  
	-${CLINKER} -o properties properties.o  ${PETSC_MAT_LIB}
	${RM} properties.o

