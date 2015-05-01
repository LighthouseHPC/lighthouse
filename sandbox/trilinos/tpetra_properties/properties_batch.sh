#!/bin/bash

#SBATCH -J test_job
#SBATCH --qos=janus
#SBATCH -t 24:00:00
#SBATCH -N 2
#SBATCH --ntasks-per-node=12
#SBATCH -o 1-true.out

unset I_MPI_PMI_LIBRARY
mkdir /lustre/janus_scratch/pamo8800/24_proc_herm
for file in /lustre/janus_scratch/pamo8800/MM_ALL_UNTAR/*.mtx
do
	timeout 5m mpirun -bootstrap slurm ./tpetra_properties_crsmatrix $file 24_proc_herm
done
