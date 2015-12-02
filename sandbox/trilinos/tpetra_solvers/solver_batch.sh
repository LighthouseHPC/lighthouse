#!/bin/bash

#SBATCH -J test_job
#SBATCH --qos=janus
#SBATCH -t 24:00:00
#SBATCH -N 1
#SBATCH --ntasks-per-node=12
#SBATCH -o belos_ifpack2.out

unset I_MPI_PMI_LIBRARY
fldr=/lustre/janus_scratch/pamo8800/belos_ifpack2_results/12_proc_n-z
mkdir $fldr
for file in /lustre/janus_scratch/pamo8800/MM_ALL_UNTAR/[n-zN-Z]*.mtx
do
	timeout 5m mpirun -bootstrap slurm ./tpetra_solvers $file $fldr
done
