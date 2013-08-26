
FBIN=$1
PROBTYPE=$2
DATA=$3

VAR_WHICH="eps_largest_magnitude eps_smallest_magnitude eps_largest_real eps_smallest_real"

if [ "$DATA" = "complex" ];then
    VAR_WHICH="eps_largest_magnitude eps_smallest_magnitude eps_largest_real eps_smallest_real eps_largest_imaginary eps_smallest_imaginary"
fi


for NPROC in 4 8
do
    for WHICH in $VAR_WHICH
	do
	    for NEIGEN in 1 2 10
	    do
		for TOL in 0.0001 0.00000001 0.0000000001
		do
		  if [ "$WHICH" = "eps_largest_magnitude" ];then # power and subspace only work for Largest magnitude
		   mpirun -np $NPROC ./eigenvalue_parallel -fin $FBIN -eps_type power -eps_tol $TOL -eps_nev $NEIGEN -$PROBTYPE
		   mpirun -np $NPROC ./eigenvalue_parallel -fin $FBIN -eps_type subspace -eps_tol $TOL -eps_nev $NEIGEN -$PROBTYPE
		  fi
		   mpirun -np $NPROC ./eigenvalue_parallel -fin $FBIN -eps_type arnoldi  -eps_tol $TOL -eps_nev $NEIGEN -$WHICH -$PROBTYPE
		  if [ "$PROBTYPE" = "1" ];then 
		   mpirun -np $NPROC ./eigenvalue_parallel -fin $FBIN -eps_type lanczos -eps_tol $TOL -eps_nev $NEIGEN -$WHICH # for HEP
		  fi
		   mpirun -np $NPROC ./eigenvalue_parallel -fin $FBIN -eps_type krylovschur -eps_tol $TOL -eps_nev $NEIGEN -$WHICH -$PROBTYPE
		   mpirun -np $NPROC ./eigenvalue_parallel -fin $FBIN -eps_type gd -eps_tol $TOL -eps_nev $NEIGEN -$WHICH -$PROBTYPE
		   mpirun -np $NPROC ./eigenvalue_parallel -fin $FBIN -eps_type jd -eps_tol $TOL -eps_nev $NEIGEN -$WHICH -$PROBTYPE
		   #mpirun -np $NPROC ./eigenvalue_parallel -fin $FBIN -eps_type rqcg -eps_tol $TOL -eps_nev $NEIGEN -$WHICH -$PROBTYPE
		done
	    done
	done
done
