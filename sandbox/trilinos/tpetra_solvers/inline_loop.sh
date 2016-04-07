#!/bin/bash

#SBATCH --job-name=UF.1000+nnz.np12
#SBATCH --qos=janus
#SBATCH --time=02:00:00
#SBATCH --nodes 1
#SBATCH --ntasks-per-node=12
#SBATCH --output=/lustre/janus_scratch/pamo8800/All_Results/slurm-%j.out

DATE=`date +%m.%d.%Y_%H:%M`
OUTDIR="/lustre/janus_scratch/pamo8800/All_Results/${SLURM_JOB_NAME}__${DATE}__${SLURM_JOB_ID}"
MATDIR=/lustre/janus_scratch/pamo8800/UF_Collection_Matrix-Market
EXEDIR=~/project/lighthouse/sandbox/trilinos/tpetra_solvers

mkdir -p $OUTDIR

while read -r f <&3
do
    echo $f
    mpirun -np 12 $EXEDIR/tpetra_solvers $MATDIR/$f -f $OUTDIR/$f.out
done 3< $1
