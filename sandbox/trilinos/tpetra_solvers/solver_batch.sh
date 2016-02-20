#!/bin/bash

#SBATCH --job-name=test_job
#SBATCH --qos=janus
#SBATCH --time=24:00:00
#SBATCH --nodes 1
#SBATCH --ntasks-per-node=12
#SBATCH --output=belos_ifpack2.out
##SBATCH --acount=

mpirun -bootstrap slurm ./tpetra_solvers ../demo.mtx
