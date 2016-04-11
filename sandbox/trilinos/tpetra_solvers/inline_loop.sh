#!/bin/bash

#SBATCH --job-name=UF.1000+nnz.np1
#SBATCH --qos=janus
#SBATCH --time=08:00:00
#SBATCH --reservation=janus-serial
#SBATCH --nodes 1
#SBATCH --ntasks-per-node=1
#SBATCH --output=/lustre/janus_scratch/pamo8800/All_Results/slurm-%j.out

DATE=`date +%m.%d.%Y_%H:%M`
OUTDIR="/lustre/janus_scratch/pamo8800/All_Results/${SLURM_JOB_NAME}__${DATE}__${SLURM_JOB_ID}"
MATDIR=/lustre/janus_scratch/pamo8800/UF_Collection_Matrix-Market
EXEDIR=~/project/lighthouse/sandbox/trilinos/tpetra_solvers

INPUT=$1

mkdir -p $OUTDIR

OLDIFS=$IFS
IFS=,
[ ! -f $INPUT ] && { echo "$INPUT file not found"; exit 99; }
while read matrix solved <&3
do
    if [ $solved -eq 0 ]
    then
        echo solving $matrix
        mpirun $EXEDIR/tpetra_solvers $MATDIR/$matrix -f $OUTDIR/$matrix.out && sed -i "s/${matrix}, 0/${matrix}, 1/g" "$1"
    else
        echo skipping $matrix
    fi
done 3< $INPUT
IFS=$OLDIFS
