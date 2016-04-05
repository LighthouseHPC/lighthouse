#!/bin/bash

while read matrix 
    do 
         echo ${matrix};  
         sbatch \
         -o /lustre/janus_scratch/pamo8800/All_Results/UF_np12_3-31-16/${matrix}.out \
         -e /lustre/janus_scratch/pamo8800/All_Results/UF_np12_3-31-16/${matrix}.err \
         /projects/pamo8800/lighthouse/sandbox/trilinos/tpetra_solvers/solver_batch_12.sh ${matrix}; 
         sleep 1; 
    done < ./UF_matrix_list_1000+nnz.txt

