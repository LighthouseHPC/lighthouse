include ${PETSC_DIR}/conf/variables
include ${PETSC_DIR}/conf/rules

ANAMOD_DIR = ${PETSC_DIR}/${PETSC_ARCH}
ANAMOD_SRC_DIR = /disks/soft/anamod2-petsc3.3
SLEPC_LIB_DIR = ${ANAMOD_SRC_DIR}/slepc-3.3-p2/${PETSC_ARCH}/lib

CC_FLAGS += -I${PETSC_DIR}/include

ANAMOD_LIB += -L${ANAMOD_DIR}/lib -Wl,-rpath,${ANAMOD_DIR}/lib \
    -lsalsamodules -lothermodules -lanamodpetsc -lanamod \
    -L${SLEPC_LIB_DIR} -Wl,-rpath,${SLEPC_LIB_DIR} -lslepc -lnmd

petscmetadataut: petscmetadataut.o  chkopts
	-${CLINKER} -o petscmetadataut petscmetadataut.o ${ANAMOD_LIB} ${PETSC_MAT_LIB} 
	${RM} petscmetadataut.o

include ${PETSC_DIR}/conf/test
