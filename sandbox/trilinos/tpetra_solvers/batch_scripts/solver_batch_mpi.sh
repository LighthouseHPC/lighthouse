#!/bin/bash

#SBATCH --job-name=10k_test
#SBATCH --qos=janus-debug
#SBATCH --time=00:10:00
#SBATCH --nodes 1
#SBATCH --ntasks-per-node=12
#SBATCH --output=10k_test.out
#SBATCH --error=10k_test.out

~/project/trilinos-petsc-testbed/openmpi-install/bin/mpirun ./tpetra_solvers /home/pamo8800/trilinos/rotor2_unsym.mtx
~/project/trilinos-petsc-testbed/openmpi-install/bin/mpirun ./tpetra_solvers /home/pamo8800/trilinos/filter2D_sym.mtx
~/project/trilinos-petsc-testbed/openmpi-install/bin/mpirun ./tpetra_solvers /home/pamo8800/trilinos/bcsstk08_spd.mtx

#~/project/trilinos-petsc-testbed/openmpi-install/bin/mpirun ./tpetra_solvers /home/pamo8800/lustre/UF_Collection_Matrix-Market/b1_ss.mtx
#~/project/trilinos-petsc-testbed/openmpi-install/bin/mpirun ./tpetra_solvers ~/lustre/moose/training_data/modules/richards/tests/gravity_head_1/gh23_3_2.mat.mtx
#~/project/trilinos-petsc-testbed/openmpi-install/bin/mpirun ./tpetra_solvers /home/pamo8800/lustre/UF_Collection_Matrix-Market/rdb200l.mtx
#~/project/trilinos-petsc-testbed/openmpi-install/bin/mpirun ./tpetra_solvers /home/pamo8800/lustre/UF_Collection_Matrix-Market/adder_dcop_11.mtx
#~/project/trilinos-petsc-testbed/openmpi-install/bin/mpirun ./tpetra_solvers /home/pamo8800/lustre/UF_Collection_Matrix-Market/ex8.mtx
#~/project/trilinos-petsc-testbed/openmpi-install/bin/mpirun ./tpetra_solvers /home/pamo8800/lustre/UF_Collection_Matrix-Market/bcsstk39.mtx
#~/project/trilinos-petsc-testbed/openmpi-install/bin/mpirun ./tpetra_solvers /home/pamo8800/lustre/UF_Collection_Matrix-Market/ML_Geer.mtx
