
if [ -z $PETSC_DIR ]; then
   echo "Cannot find environment variable PETSC_DIR, exiting.."
   exit 1
fi

if [ -z $PETSC_ARCH ]; then
   echo "Cannot find environment variable PETSC_ARCH, exiting.."
   exit 1
fi

if [ -z $SLEPC_DIR ]; then
   echo "Cannot find environment variable SLEPC_DIR, exiting.."
   exit 1
fi

make eigenvalue_parallel
make matrix_load

#Create a binary folder where we can dump all the 
#binary produced files for use in our experiments
BIN_FOLDER="binary"
if [ ! -e $BIN_FOLDER ]; then
   mkdir $BIN_FOLDER
fi

#Create a results folder where we can save all the 
#data results 
RESULTS="result"
if [ ! -e $RESULTS ]; then
   mkdir $RESULTS
fi

CURR_DIR=${PWD}
echo ${CURR_DIR}


# the configuration for real matrices---------------------
cd "${PETSC_DIR}"
pwd
#unset PETSC_ARCH
./configure --with-cc=gcc --download-f2cblaslapack --download-mpich 
#export PETSC_ARCH=arch-linux2-c-opt
make all 
cd "${SLEPC_DIR}"
./configure
make
cd "${CURR_DIR}"
unset PETSC_USE_COMPLEX
#-----------------------------------------------------------


WHICH=real
for FILENAME in bcsstm39.mtx shipsec1.mtx af23560.mtx stomach.mtx
do
    PROBTYPE=eps_hermitian

    if [ $FILENAME="af23560.mtx" -o $FILENAME="stomach.mtx" ];then
	PROBTYPE=eps_non_hermitian
    fi

    FBIN=${BIN_FOLDER}/${FILENAME}_Binary
    FDATA=${RESULTS}/${FILENAME}_Data

    ./matrix_load -fin $FILENAME -fout $FBIN -c 0
    ./experiments.sh ${FBIN} ${PROBTYPE} ${WHICH} >${FDATA}

done

# the configuration for complex matrices---------------------
cd "${PETSC_DIR}"
pwd

./configure --with-cc=gcc --download-f2cblaslapack --download-mpich --with-scalar-type=complex

make all 
cd "${SLEPC_DIR}"
./configure
make
cd "${CURR_DIR}"
export PETSC_USE_COMPLEX=1
#-----------------------------------------------------------

WHICH=complex
for FILENAME in mhd1280a.mtx mhd1280b.mtx kim1.mtx 
do
    PROBTYPE=eps_non_hermitian

    if [ $FILENAME="mhd1280b.mtx" ];then
	PROBTYPE=eps_hermitian
    fi

    FBIN=${BIN_FOLDER}/${FILENAME}_Binary
    FDATA=${RESULTS}/${FILENAME}_Data

    ./matrix_load -fin $FILENAME -fout $FBIN -c 1
    ./experiments.sh ${FBIN} ${PROBTYPE} ${WHICH} >${FDATA}

done
