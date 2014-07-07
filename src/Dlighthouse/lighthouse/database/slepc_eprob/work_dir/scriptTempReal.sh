
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

CURR_DIR=${PWD}

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

