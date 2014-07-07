
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

