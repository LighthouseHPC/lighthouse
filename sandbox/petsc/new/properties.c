static char help[] = "Computes and prints various properties\n\
											a real square matrix (matrix market format).\n\
											-f <input_file> : matrix market file\n\n";

#include <petscmat.h>
#include <petsctime.h>

extern PetscErrorCode Dimension(Mat,PetscInt*,PetscInt*);
extern PetscErrorCode BlockSize(Mat,PetscInt*);
extern PetscErrorCode Nonzeros(Mat,PetscInt*);
extern PetscErrorCode MaxNonzerosPerRow(Mat,PetscInt*);
extern PetscErrorCode MinNonzerosPerRow(Mat,PetscInt*);
extern PetscErrorCode AvgNonzerosPerRow(Mat,PetscInt*);
extern PetscErrorCode DummyRows(Mat,PetscInt*);
extern PetscErrorCode DummyRowsKind(Mat,PetscInt*);
extern PetscErrorCode AbsoluteNonZeroSum(Mat,PetscScalar*);  
extern PetscErrorCode NumericValueSymmetryV1(Mat,PetscInt*);
extern PetscErrorCode NonZeroPatternSymmetryV1(Mat,PetscInt*);
extern PetscErrorCode NumericValueSymmetryV2(Mat,PetscScalar*);
extern PetscErrorCode NonZeroPatternSymmetryV2(Mat,PetscScalar*);
extern PetscErrorCode Trace(Mat,PetscScalar*);
extern PetscErrorCode AbsoluteTrace(Mat,PetscScalar*);
extern PetscErrorCode OneNorm(Mat,PetscScalar*);
extern PetscErrorCode InfinityNorm(Mat,PetscScalar*);
extern PetscErrorCode FrobeniusNorm(Mat,PetscScalar*);
extern PetscErrorCode SymmetricInfinityNorm(Mat,PetscScalar*);
extern PetscErrorCode SymmetricFrobeniusNorm(Mat,PetscScalar*);
extern PetscErrorCode AntiSymmetricInfinityNorm(Mat,PetscScalar*);
extern PetscErrorCode AntiSymmetricFrobeniusNorm(Mat,PetscScalar*);
extern PetscErrorCode RowDiagonalDominance(Mat,PetscInt*); 
extern PetscErrorCode ColumnDiagonalDominance(Mat,PetscInt*); 
extern PetscErrorCode RowVariance(Mat,PetscScalar*);
extern PetscErrorCode ColumnVariance(Mat,PetscScalar*);
extern PetscErrorCode DiagonalAverage(Mat,PetscScalar*);
extern PetscErrorCode DiagonalVariance(Mat,PetscScalar*);
extern PetscErrorCode DiagonalSign(Mat,PetscInt*);
extern PetscErrorCode DiagonalNonZeros(Mat,PetscInt*);
extern PetscErrorCode lowerBandwidth(Mat,PetscInt*);
extern PetscErrorCode upperBandwidth(Mat,PetscInt*);


#undef __FUNCT__
#define __FUNCT__ "main"
int main(int argc,char **args)
{
	Mat            A,B; // we'll compute proerties of A, we'll use B for help
	Vec            d;      /* vector for fixing missing diagonal entries */
	char           filein[PETSC_MAX_PATH_LEN],buf[PETSC_MAX_PATH_LEN];
	PetscInt       i,m,n,k,nnz,col,row;
	PetscErrorCode ierr;
	PetscMPIInt    size;
	PetscScalar    val,one = 1;
	FILE*          file;
	PetscLogDouble compTime,startTime,endTime;

	PetscInitialize(&argc,&args,(char *)0,help);

	ierr = MPI_Comm_size(PETSC_COMM_WORLD,&size);CHKERRQ(ierr);
	if (size > 1) SETERRQ(PETSC_COMM_WORLD,1,"This program cannot be run in parallel!\n");

	/* Read in matrix market matrix */
	ierr = PetscOptionsGetString(PETSC_NULL,"-f",filein,PETSC_MAX_PATH_LEN,PETSC_NULL);CHKERRQ(ierr);
	ierr = PetscFOpen(PETSC_COMM_SELF,filein,"r",&file);CHKERRQ(ierr);

	/* process header with comments */
	PetscBool isSymmetric = PETSC_FALSE;
	do {
		fgets(buf,PETSC_MAX_PATH_LEN-1,file);
		// Check if matrix is symmetric
		if(strstr(buf, "symmetric") != NULL){
			isSymmetric = PETSC_TRUE;
		}
	}while (buf[0] == '%');

	/* The first non-comment line has the matrix dimensions */
	sscanf(buf,"%d %d %d\n",&m,&n,&nnz);

	if(isSymmetric){
		ierr = MatCreateSeqAIJ(PETSC_COMM_WORLD,m,n,nnz*2/m,0,&A);CHKERRQ(ierr);  
		ierr = MatCreateSeqAIJ(PETSC_COMM_WORLD,m,n,nnz*2/m,0,&B);CHKERRQ(ierr);  
	}else{
		ierr = MatCreateSeqAIJ(PETSC_COMM_WORLD,m,n,nnz/m,0,&A);CHKERRQ(ierr);  
		ierr = MatCreateSeqAIJ(PETSC_COMM_WORLD,m,n,nnz/m,0,&B);CHKERRQ(ierr);  
	}

	ierr = MatSetOption(A,MAT_NEW_NONZERO_ALLOCATION_ERR,PETSC_FALSE);CHKERRQ(ierr);
	ierr = MatSetOption(B,MAT_NEW_NONZERO_ALLOCATION_ERR,PETSC_FALSE);CHKERRQ(ierr);

	// Create vector d for fixing missing diagonal entries
	ierr = VecCreate(PETSC_COMM_WORLD,&d);CHKERRQ(ierr);
	ierr = VecSetSizes(d,PETSC_DECIDE,n);CHKERRQ(ierr);
	ierr = VecSetFromOptions(d);CHKERRQ(ierr);

	// Set all entries to 0
	ierr = VecSet(d,0);CHKERRQ(ierr);

	// For every nonzero entry in A, we'll put a 1 in B
	// We'll use B to evaluate nonzero pattern symmetry
	for (i=0; i<nnz; i++) {
		fscanf(file,"%d %d %le\n",&row,&col,(double*)&val);
		row = row-1; col = col-1 ;
		ierr = MatSetValues(A,1,&row,1,&col,&val,INSERT_VALUES);CHKERRQ(ierr);
		ierr = MatSetValues(B,1,&row,1,&col,&one,INSERT_VALUES);CHKERRQ(ierr);
		if(isSymmetric){
			if (row != col){
				ierr = MatSetValues(A,1,&col,1,&row,&val,INSERT_VALUES);CHKERRQ(ierr);
				ierr = MatSetValues(B,1,&col,1,&row,&one,INSERT_VALUES);CHKERRQ(ierr);
			}
		}
		// If matrix has diagonal entry, 
		// set that entry of vector d to 1
		if(row == col) 
			VecSetValue(d,row,1,ADD_VALUES);
	}

	VecAssemblyBegin(d); VecAssemblyEnd(d);
	PetscScalar dv[1];
	val = 0;
	for(k=0; k<m; k++){
		VecGetValues(d,1,&k,dv);
		// If an entry in vector d is zero, 
		// matrix A is missing that diagonal entry.
		if(dv[0] == 0) // Insert a 0 at that location of A
			ierr = MatSetValues(A,1,&k,1,&k,&val,INSERT_VALUES);CHKERRQ(ierr);
	}

	fclose(file);

	ierr = MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
	ierr = MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
	ierr = MatAssemblyBegin(B,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
	ierr = MatAssemblyEnd(B,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);

	/* Compute and print matrix properties */ 

	PetscInt r=0,c=0,nz=0;
	PetscScalar x;

	ierr = PetscTime(&startTime);CHKERRQ(ierr);

  printf("%s, ", filein);
  printf("%d, %d, ", m, n);

	RowVariance(A,&x);
  //printf ("Row variance: %G\n",x);
  printf("%G ,", x); 

	ColumnVariance(A,&x);
  //printf ("Column variance: %G\n",x);
	printf("%G ,", x);

	DiagonalVariance(A,&x);
	//printf ("Diagonal variance: %g\n",x);
  printf("%g ,", x);

	Nonzeros(A,&nz);
	//printf ("Nonzeros: %d\n",nz);
  printf("%d ,", nz);

	Dimension(A,&r,&c);
	//printf ("Rows: %d\n",r);
	printf("%d ,", r);

	FrobeniusNorm(A,&x);
	//printf ("Frobenius norm: %G\n",x);
  printf("%G ,", x);

	SymmetricFrobeniusNorm(A,&x);
	//printf ("Frobenius norm of symmetric part: %G\n",x);
  printf("%G ,", x);

	AntiSymmetricFrobeniusNorm(A,&x);
	//printf ("Frobenius norm of antisymmetric part: %G\n",x);
  printf("%G ,", x);

	OneNorm(A,&x);
	//printf ("One norm: %G\n",x);
  printf("%G ,", x);

	InfinityNorm(A,&x);
	//printf ("Infinity norm: %G\n",x);
  printf("%G ,", x);

	SymmetricInfinityNorm(A,&x);
	//printf ("Inifinity norm of symmetric part: %G\n",x);
  printf("%G ,", x);

	AntiSymmetricInfinityNorm(A,&x);
	//printf ("Inifinity norm of antisymmetric part: %G\n",x);
  printf("%G ,", x);

	MaxNonzerosPerRow(A,&nz);
	//printf ("Max. nonzeros per row: %d\n",nz);
  printf("%d ,", nz);

	Trace(A,&x);
	//printf ("Trace: %G\n",x);
  printf("%G ,", x);

	AbsoluteTrace(A,&x);
	//printf ("Absolute trace: %G\n",x);
  printf("%G ,", x);

	MinNonzerosPerRow(A,&nz);
	//printf ("Min. nonzeros per row: %d\n",nz);
  printf("%d ,", nz);

	AvgNonzerosPerRow(A,&nz);
	//printf ("Avg. nonzeros per row: %d\n",nz);
  printf("%d ,", nz);

	DummyRows(A,&nz);
	//printf ("Number of dummy rows: %d\n",nz);
  printf("%d ,", nz);

	DummyRowsKind(A,&nz);
	//printf ("Dummy rows kind: %d\n",nz);
  printf("%d ,", nz);

	NumericValueSymmetryV1(A,&r);
	//printf ("Numerical Symmetry: %d\n",r);
  printf("%d ,", r);

	NonZeroPatternSymmetryV1(B,&r);
	//printf ("Structural Symmetry: %d\n",r);
  printf("%d ,", r);

	NumericValueSymmetryV2(A,&x);
	//printf ("Numeric value Symmetry: %G\n",x);
  printf("%G ,", x);

	NonZeroPatternSymmetryV2(B,&x);
	//printf ("Nonzero pattern Symmetry: %G\n",x);
  printf("%G ,", x);

	RowDiagonalDominance(A,&r);
	//printf ("Row Diagonal Dominance: %d\n",r);
  printf("%d ,", r);

	ColumnDiagonalDominance(A,&r);
	//printf ("Column Diagonal Dominance: %d\n",r);
  printf("%d ,", r);

	DiagonalAverage(A,&x);
	//printf ("Diagonal average: %g\n",x);
  printf("%g ,", x);

	DiagonalSign(A,&r);
	//printf ("Diagonal sign: %d\n",r);
  printf("%d ,", r);

	DiagonalNonZeros(A,&r);
	//printf ("Diagonal nonzero count: %d\n",r);
  printf("%d ,", r);

	lowerBandwidth(A, &r);
	//printf ("Lower bandwidth: %d\n",r);
  printf("%d ,", r);

	upperBandwidth(A, &r);
	//printf ("Upper bandwidth: %d\n",r);
  printf("%d", r);
  printf("\n");

	/* Destroy matrices and finalize PETSc */ 
	ierr = PetscTime(&endTime);CHKERRQ(ierr);

	// Find computation time
	compTime = endTime - startTime;
	//printf("%e\n",compTime);

	ierr = MatDestroy(&A);CHKERRQ(ierr);
	ierr = MatDestroy(&B);CHKERRQ(ierr);

	ierr = PetscFinalize();
	return(0);
}

// number of rows and columns
PetscErrorCode Dimension(Mat M, PetscInt *m, PetscInt *n)
{
	PetscErrorCode ierr;
	ierr = MatGetSize(M,m,n);CHKERRQ(ierr);
	return(0);
}

// block size of the matrix
PetscErrorCode BlockSize(Mat M, PetscInt *blockSize)
{
	PetscErrorCode ierr;
	MatInfo mi;
	ierr = MatGetInfo(M,MAT_GLOBAL_SUM,&mi);CHKERRQ(ierr);
	*blockSize = (PetscInt)mi.block_size;
	return(0);
}

// finds the total number of nonzeros
PetscErrorCode Nonzeros(Mat M, PetscInt *nonzeros)
{
	PetscInt m,n,i,j,nc,nnz;
	PetscErrorCode ierr;  

	ierr = Dimension(M, &m, &n);CHKERRQ(ierr);  
	nnz = 0;
	for (i=0; i<m; i++) {
		const PetscInt *cols[n];
		const PetscScalar *vals[n];
		ierr = MatGetRow(M,i,&nc,cols,vals);CHKERRQ(ierr);
		if(nc != 0){      
			for(j=0;j<nc;j++){                   
				if(*(vals[0]+j) != 0) nnz++;
			}
		}
		ierr = MatRestoreRow(M,i,&nc,cols,vals);CHKERRQ(ierr);
	}
	*nonzeros = nnz;
	return(0);
}

// finds the maximum number of nonzeros per row
PetscErrorCode MaxNonzerosPerRow(Mat M, PetscInt *maxNz)
{
	PetscErrorCode ierr;
	PetscInt m, n, i, j, nnz, nc;

	*maxNz = 0;
	ierr = Dimension(M, &m, &n);CHKERRQ(ierr);

	for(i = 0; i < m; i++){
		const PetscInt *cols[n];
		const PetscScalar *vals[n];
		ierr = MatGetRow(M,i,&nc,cols,vals);CHKERRQ(ierr);
		nnz = 0;
		if(nc != 0){      
			for(j=0;j<nc;j++){                   
				if(*(vals[0]+j) != 0) nnz++;
			}
		}
		if(nnz > *maxNz){
			*maxNz = nnz;
		}    
		ierr = MatRestoreRow(M,i,&nc,cols,vals);CHKERRQ(ierr);
	}
	return(0);
}

// finds the minimum number of nonzeros per row
PetscErrorCode MinNonzerosPerRow(Mat M, PetscInt *minNz)
{
	PetscErrorCode ierr;
	PetscInt m, n, i, j, nnz, nc;

	*minNz = n;
	ierr = Dimension(M, &m, &n);CHKERRQ(ierr);

	for(i = 0; i < m; i++){
		const PetscInt *cols[n];
		const PetscScalar *vals[n];
		ierr = MatGetRow(M,i,&nc,cols,vals);CHKERRQ(ierr);
		nnz = 0;
		if(nc != 0){      
			for(j=0;j<nc;j++){                   
				if(*(vals[0]+j) != 0) nnz++;
			}
		}
		if(nnz < *minNz){
			*minNz = nnz;
		}    
		ierr = MatRestoreRow(M,i,&nc,cols,vals);CHKERRQ(ierr);
	}
	return(0);
}

// finds the average number of nonzeros per row
PetscErrorCode AvgNonzerosPerRow(Mat M, PetscInt *avgNz)
{
	PetscErrorCode ierr;
	PetscInt m, n, i, j, nc, nnz;
	const PetscInt *cols[n];
	const PetscScalar *vals[n];
	PetscScalar sum=0;
	ierr = Dimension(M, &m, &n);CHKERRQ(ierr);

	for(i = 0; i < m; i++){
		ierr = MatGetRow(M,i,&nc,cols,vals);CHKERRQ(ierr);
		nnz = 0;
		if(nc != 0){      
			for(j=0;j<nc;j++){                   
				if(*(vals[0]+j) != 0) nnz++;
			}
		}
		if(nnz != 0){
			sum = sum + nnz;
		}  
		ierr = MatRestoreRow(M,i,&nc,cols,vals);CHKERRQ(ierr);
	}
	*avgNz = sum/(PetscScalar)m;
	return(0);
}

// finds the number of rows with only one element
PetscErrorCode DummyRows(Mat M, PetscInt *dummyRows)
{
	PetscErrorCode ierr;
	PetscInt m, n, i, nz;
	const PetscInt *cols[n];
	const PetscScalar *vals[n];

	ierr = Dimension(M, &m, &n);CHKERRQ(ierr);
	*dummyRows = 0;

	for(i = 0; i < m; i++){
		ierr = MatGetRow(M,i,&nz,cols,vals);CHKERRQ(ierr);
		if(nz == 1){
			if(*(vals[0]) != 0) *dummyRows += 1;      
		}
		ierr = MatRestoreRow(M,i,&nz,cols,vals);CHKERRQ(ierr);
	}
	return(0);
}

// 0 if all dummy rows have a one on the diagonal,
// 1 if the value is not one, 2 if not on the diagonal
PetscErrorCode DummyRowsKind(Mat M, PetscInt *dummyRowsKind)
{
	PetscErrorCode ierr;
	PetscInt m, n, i, nz;
	PetscInt kind_0=0, kind_1=0, kind_2=0, nDumRows=0;
	ierr = Dimension(M, &m, &n);CHKERRQ(ierr);

	for(i = 0; i < m; i++){
		const PetscInt *cols[n];
		const PetscScalar *vals[n];
		ierr = MatGetRow(M,i,&nz,cols,vals);CHKERRQ(ierr);
		if(nz == 1){
			if(*cols[0] == i){
				if(*vals[0] == 1) kind_0++;
				else kind_1++;
			}
			else kind_2++;
			nDumRows++;
		}
		ierr = MatRestoreRow(M,i,&nz,cols,vals);CHKERRQ(ierr);
	}

	if(kind_0 == nDumRows && kind_1 == 0 && kind_2 == 0) *dummyRowsKind = 0;
	if(kind_1 > 0 && kind_2 == 0) *dummyRowsKind = 1;
	if(kind_2 > 0) *dummyRowsKind = 2;
	return(0);
}

// computes the sum of the absolute values 
// of all the nonzeros in a matrix
PetscErrorCode AbsoluteNonZeroSum(Mat M, PetscScalar *asum){  
	PetscScalar absum; 
	PetscInt m,n,i,j,nc;
	PetscErrorCode ierr;  

	ierr = Dimension(M, &m, &n);CHKERRQ(ierr);  
	absum = 0;
	for (i=0; i<m; i++) {
		const PetscInt *cols[n];
		const PetscScalar *vals[n];
		ierr = MatGetRow(M,i,&nc,cols,vals);CHKERRQ(ierr);
		if(nc != 0){      
			for(j=0;j<nc;j++){                   
				absum = absum + PetscAbsScalar(*(vals[0]+j));
			}
		}
		ierr = MatRestoreRow(M,i,&nc,cols,vals);CHKERRQ(ierr);
	}
	*asum = absum;
	return(0);
}

// checks the numerical symmetry, if symmetric returns 1
PetscErrorCode NumericValueSymmetryV1(Mat A, PetscInt *s)
{
	PetscErrorCode ierr;
	Mat Atrans;
	PetscBool b;
	*s = 0;
	ierr = MatTranspose(A, MAT_INITIAL_MATRIX,&Atrans);CHKERRQ(ierr);
	ierr = MatEqual(A, Atrans, &b);CHKERRQ(ierr);
	ierr = MatDestroy(&Atrans);CHKERRQ(ierr);
	if(b) *s = 1;
	return(0);
}

// checks the nonzero pattern symmetry, if symmetric returns 1
PetscErrorCode NonZeroPatternSymmetryV1(Mat B, PetscInt *s)
{
	PetscErrorCode ierr;
	Mat Btrans;
	PetscBool b;
	*s = 0;
	ierr = MatTranspose(B, MAT_INITIAL_MATRIX,&Btrans);CHKERRQ(ierr);
	ierr = MatEqual(B, Btrans, &b);CHKERRQ(ierr);
	ierr = MatDestroy(&Btrans);CHKERRQ(ierr);  
	if(b) *s = 1;
	return(0);
}

// checks the numerical symmetry, returns percentage
PetscErrorCode NumericValueSymmetryV2(Mat A, PetscScalar *s)
{
	PetscErrorCode ierr;
	Mat S,T;
	PetscScalar nzsS,nzsA;
	ierr = MatTranspose(A,MAT_INITIAL_MATRIX,&T);CHKERRQ(ierr);
	ierr = MatDuplicate(T,MAT_SHARE_NONZERO_PATTERN,&S);CHKERRQ(ierr);
	ierr = MatAXPY(S,-0.5,T,SAME_NONZERO_PATTERN);CHKERRQ(ierr);
	ierr = MatAXPY(S,0.5,A,DIFFERENT_NONZERO_PATTERN);CHKERRQ(ierr);
	ierr = AbsoluteNonZeroSum(S,&nzsS);
	ierr = AbsoluteNonZeroSum(A,&nzsA);
	*s = 1-((nzsS/2)/nzsA);
	ierr = MatDestroy(&S);CHKERRQ(ierr);
	ierr = MatDestroy(&T);CHKERRQ(ierr);
	return(0);
}

// checks the nonzero pattern symmetry, returns percentage
PetscErrorCode NonZeroPatternSymmetryV2(Mat B, PetscScalar *s)
{
	PetscErrorCode ierr;
	Mat S,T;
	PetscScalar nzS,nzB;
	ierr = MatTranspose(B,MAT_INITIAL_MATRIX,&T);CHKERRQ(ierr);
	ierr = MatDuplicate(T,MAT_SHARE_NONZERO_PATTERN,&S);CHKERRQ(ierr);
	ierr = MatAXPY(S,-1,T,SAME_NONZERO_PATTERN);CHKERRQ(ierr);
	ierr = MatAXPY(S,1,B,DIFFERENT_NONZERO_PATTERN);CHKERRQ(ierr);
	ierr = AbsoluteNonZeroSum(S,&nzS);
	ierr = AbsoluteNonZeroSum(B,&nzB);
	*s = 1-((nzS/2)/nzB);
	ierr = MatDestroy(&S);CHKERRQ(ierr);
	ierr = MatDestroy(&T);CHKERRQ(ierr);
	return(0);
}

// computes trace (sum of diagonal elements)
PetscErrorCode Trace(Mat M, PetscScalar *trace){
	PetscErrorCode ierr;
	ierr = MatGetTrace(M,trace);CHKERRQ(ierr);
	return 0;
}

// computes absolute trace (sum of absolute values of the diagonal elements)
PetscErrorCode AbsoluteTrace(Mat M, PetscScalar *trace){
	Vec D; 
	PetscScalar *d;
	PetscScalar sum; 
	PetscInt m,n,i;
	PetscErrorCode ierr;

	ierr = Dimension(M, &m, &n);CHKERRQ(ierr);
	ierr = VecCreate(PETSC_COMM_WORLD,&D);CHKERRQ(ierr);
	ierr = VecSetSizes(D,PETSC_DECIDE,n);CHKERRQ(ierr);
	ierr = VecSetFromOptions(D);CHKERRQ(ierr);

	ierr = MatGetDiagonal(M,D); CHKERRQ(ierr);
	ierr = VecGetArray(D,&d); CHKERRQ(ierr);
	sum = 0;
	for (i=0; i<n; i++) {
		sum += PetscAbsScalar(d[i]);
	}
	ierr = VecRestoreArray(D,&d); CHKERRQ(ierr);
	ierr = VecDestroy(&D); CHKERRQ(ierr);
	*trace = sum;
	return 0;
}

// computes one norm of a matrix
PetscErrorCode OneNorm(Mat M, PetscScalar *norm){
	PetscErrorCode ierr;
	ierr = MatNorm(M,NORM_1,norm);CHKERRQ(ierr);
	return 0;
}

// computes frobenius norm of a matrix
PetscErrorCode FrobeniusNorm(Mat M, PetscScalar *norm){
	PetscErrorCode ierr;
	ierr = MatNorm(M,NORM_FROBENIUS,norm);CHKERRQ(ierr);
	return 0;
}

// computes infinity norm of the matrix
PetscErrorCode InfinityNorm(Mat M, PetscScalar *norm){
	PetscErrorCode ierr;
	ierr = MatNorm(M,NORM_INFINITY,norm);CHKERRQ(ierr);
	return 0;
}

// computes infinity norm of the symmetric part of the matrix
PetscErrorCode SymmetricInfinityNorm(Mat A, PetscScalar *norm){
	PetscErrorCode ierr;
	Mat S,T;
	ierr = MatTranspose(A,MAT_INITIAL_MATRIX,&T);CHKERRQ(ierr);
	ierr = MatDuplicate(T,MAT_SHARE_NONZERO_PATTERN,&S);CHKERRQ(ierr);
	ierr = MatAXPY(S,0.5,T,SAME_NONZERO_PATTERN);CHKERRQ(ierr);
	ierr = MatAXPY(S,0.5,A,DIFFERENT_NONZERO_PATTERN);CHKERRQ(ierr);
	ierr = MatNorm(S,NORM_INFINITY,norm);CHKERRQ(ierr);
	ierr = MatDestroy(&S);CHKERRQ(ierr);
	ierr = MatDestroy(&T);CHKERRQ(ierr);
	return 0;
}

// computes Frobenius norm of the symmetric part of the matrix
PetscErrorCode SymmetricFrobeniusNorm(Mat A, PetscScalar *norm){
	PetscErrorCode ierr;
	Mat S,T;
	ierr = MatTranspose(A,MAT_INITIAL_MATRIX,&T);CHKERRQ(ierr);
	ierr = MatDuplicate(T,MAT_SHARE_NONZERO_PATTERN,&S);CHKERRQ(ierr);
	ierr = MatAXPY(S,0.5,T,SAME_NONZERO_PATTERN);CHKERRQ(ierr);
	ierr = MatAXPY(S,0.5,A,DIFFERENT_NONZERO_PATTERN);CHKERRQ(ierr);
	ierr = MatNorm(S,NORM_FROBENIUS,norm);CHKERRQ(ierr);
	ierr = MatDestroy(&S);CHKERRQ(ierr);
	ierr = MatDestroy(&T);CHKERRQ(ierr);
	return 0;
}

// computes infinity norm of the antisymmetric part of the matrix
PetscErrorCode AntiSymmetricInfinityNorm(Mat A, PetscScalar *norm){
	PetscErrorCode ierr;
	Mat S,T;
	ierr = MatTranspose(A,MAT_INITIAL_MATRIX,&T);CHKERRQ(ierr);
	ierr = MatDuplicate(T,MAT_SHARE_NONZERO_PATTERN,&S);CHKERRQ(ierr);
	ierr = MatAXPY(S,-0.5,T,SAME_NONZERO_PATTERN);CHKERRQ(ierr);
	ierr = MatAXPY(S,0.5,A,DIFFERENT_NONZERO_PATTERN);CHKERRQ(ierr);
	ierr = MatNorm(S,NORM_INFINITY,norm);CHKERRQ(ierr);
	ierr = MatDestroy(&S);CHKERRQ(ierr);
	ierr = MatDestroy(&T);CHKERRQ(ierr);
	return 0;
}

// computes Frobenius norm of the antisymmetric part of the matrix
PetscErrorCode AntiSymmetricFrobeniusNorm(Mat A, PetscScalar *norm){
	PetscErrorCode ierr;
	Mat S,T;
	ierr = MatTranspose(A,MAT_INITIAL_MATRIX,&T);CHKERRQ(ierr);
	ierr = MatDuplicate(T,MAT_SHARE_NONZERO_PATTERN,&S);CHKERRQ(ierr);
	ierr = MatAXPY(S,-0.5,T,SAME_NONZERO_PATTERN);CHKERRQ(ierr);
	ierr = MatAXPY(S,0.5,A,DIFFERENT_NONZERO_PATTERN);CHKERRQ(ierr);
	ierr = MatNorm(S,NORM_FROBENIUS,norm);CHKERRQ(ierr);
	ierr = MatDestroy(&S);CHKERRQ(ierr);
	ierr = MatDestroy(&T);CHKERRQ(ierr);
	return 0;
}

// computes the (row) diagonal dominance of a matrix
// returns 0 if it's not
// returns 1 if it's weakly diagonally dominant
// returns 2 if it's strictly diagonally dominant
PetscErrorCode RowDiagonalDominance(Mat M, PetscInt *dom){  
	PetscScalar ii,absum; 
	PetscInt m,n,i,j,nc, dom0=0, dom1=0, dom2=0;
	PetscErrorCode ierr;  

	ierr = Dimension(M, &m, &n);CHKERRQ(ierr);  

	for (i=0; i<m; i++) {
		absum = 0;
		ii = 0;   
		const PetscInt *cols[n];
		const PetscScalar *vals[n];
		ierr = MatGetRow(M,i,&nc,cols,vals);CHKERRQ(ierr);
		if(nc != 0){      
			for(j=0;j<nc;j++){        
				if(*(cols[0]+j) == i){         
					ii = PetscAbsScalar(*(vals[0]+j));
				}else{
					absum = absum + PetscAbsScalar(*(vals[0]+j));
				}
			}
			if(ii < absum){
				dom0 += 1;
			}
			if(ii == absum){        
				dom1 += 1;
			}
			if(ii > absum){        
				dom2 += 1;
			}
		}else{
			dom1 += 1;
		}

		if(dom0 > 0) *dom = 0;
		else if(dom0 == 0 && dom1 > 0) *dom = 1;
		else if(dom0 == 0 && dom1 == 0 && dom2 > 0) *dom = 2;
		ierr = MatRestoreRow(M,i,&nc,cols,vals);CHKERRQ(ierr);
	}
	return 0;
}

// computes the (column) diagonal dominance of a matrix
// returns 0 if it's not
// returns 1 if it's weakly diagonally dominant
// returns 2 if it's strictly diagonally dominant
PetscErrorCode ColumnDiagonalDominance(Mat M, PetscInt *dom){
	PetscErrorCode  ierr;
	Mat T;
	ierr = MatTranspose(M,MAT_INITIAL_MATRIX,&T);CHKERRQ(ierr);
	ierr = RowDiagonalDominance(T,dom);CHKERRQ(ierr);
	return 0;
}

// computes the row variance of a matrix
PetscErrorCode RowVariance(Mat M, PetscScalar* vrn){  
	PetscScalar ssum,dv[1],mean = 0,var = 0,vecSum = 0; 
	PetscInt m,n,i,j,nc;
	PetscErrorCode ierr;
	Vec rowSum, V;
	PetscScalar* d;

	ierr = Dimension(M, &m, &n);CHKERRQ(ierr);

	ierr = VecCreate(PETSC_COMM_WORLD,&rowSum);CHKERRQ(ierr);
	ierr = VecSetSizes(rowSum,PETSC_DECIDE,n);CHKERRQ(ierr);
	ierr = VecSetFromOptions(rowSum);CHKERRQ(ierr);

	ierr = VecCreate(PETSC_COMM_WORLD,&V);CHKERRQ(ierr);
	ierr = VecSetSizes(V,PETSC_DECIDE,m);CHKERRQ(ierr);
	ierr = VecSetFromOptions(V);CHKERRQ(ierr);

	ierr = MatGetRowSum(M, rowSum);CHKERRQ(ierr);

	for (i=0; i<m; i++){
		ierr = VecGetValues(rowSum,1,&i,dv);CHKERRQ(ierr);
		const PetscInt *cols[n];
		const PetscScalar *vals[n];
		ssum = 0;
		mean = dv[0]/n;
		ierr = MatGetRow(M,i,&nc,cols,vals);CHKERRQ(ierr);
		if(nc != 0){
			for(j=0;j<nc;j++){
				ssum = ssum + (*(vals[0]+j)-mean)*(*(vals[0]+j)-mean);
			}
		}else{
			ssum = 0;
		}
		var = ssum/n;
		ierr = VecSetValue(V,i,var,INSERT_VALUES);
		ierr = MatRestoreRow(M,i,&nc,cols,vals);CHKERRQ(ierr);
	}
	VecAssemblyBegin(V); VecAssemblyEnd(V);

	ierr = VecSum(V,&vecSum);
	ierr = VecGetArray(V,&d); CHKERRQ(ierr);
	mean = vecSum/m;
	ssum = 0;
	PetscScalar maxVar = 0;
	for(i=0; i<m; i++){
		if(d[i] > maxVar) maxVar = d[i];    
	}
	//*vrn = ssum/m;
	*vrn = maxVar;
	return 0;
}

// computes the column variance of a matrix
PetscErrorCode ColumnVariance(Mat M, PetscScalar* v){  
	PetscErrorCode  ierr;
	Mat T;
	ierr = MatTranspose(M,MAT_INITIAL_MATRIX,&T);CHKERRQ(ierr);
	ierr = RowVariance(T,v);CHKERRQ(ierr);
	return 0;
}

// computes the average of the absolute values
// of the diagonal elements of a matrix
PetscErrorCode DiagonalAverage(Mat M, PetscScalar* v){  
	PetscErrorCode  ierr;
	PetscScalar da = 0;
	PetscInt m,n;
	ierr = Dimension(M,&m,&n);CHKERRQ(ierr);
	ierr = AbsoluteTrace(M,&da);CHKERRQ(ierr);
	*v = da/m;
	return 0;
}

// computes the variance of 
// the diagonal elements
PetscErrorCode DiagonalVariance(Mat M, PetscScalar* dv){  
	PetscErrorCode  ierr;
	PetscScalar da=0,sum=0;
	PetscInt m,n,i;
	Vec D;

	ierr = Dimension(M,&m,&n);
	PetscScalar* d;

	ierr = DiagonalAverage(M,&da);

	ierr = VecCreate(PETSC_COMM_WORLD,&D);CHKERRQ(ierr);
	ierr = VecSetSizes(D,PETSC_DECIDE,n);CHKERRQ(ierr);
	ierr = VecSetFromOptions(D);CHKERRQ(ierr);

	ierr = MatGetDiagonal(M,D); CHKERRQ(ierr);
	ierr = VecGetArray(D,&d); CHKERRQ(ierr);
	sum = 0;
	for (i=0; i<n; i++) {
		sum += (d[i]-da)*(d[i]-da);
	}  
	ierr = VecRestoreArray(D,&d); CHKERRQ(ierr);
	ierr = VecDestroy(&D); CHKERRQ(ierr);
	*dv = sum/n;
	return 0;
}

// indicates the diagonal sign pattern
// -2 all negative, -1 nonpositive, 0 all zero, 1 nonnegative, 2 all positive, 
// 3 some negative,some or no zero,some positive
PetscErrorCode DiagonalSign(Mat M, PetscInt* ds){  
	PetscErrorCode  ierr;
	PetscInt m,n,i,an=0,az=0,ap=0;
	Vec D;

	ierr = Dimension(M,&m,&n);
	PetscScalar* d;

	ierr = VecCreate(PETSC_COMM_WORLD,&D);CHKERRQ(ierr);
	ierr = VecSetSizes(D,PETSC_DECIDE,n);CHKERRQ(ierr);
	ierr = VecSetFromOptions(D);CHKERRQ(ierr);

	ierr = MatGetDiagonal(M,D); CHKERRQ(ierr);
	ierr = VecGetArray(D,&d); CHKERRQ(ierr);

	for (i=0; i<n; i++){
		if(d[i] < 0)an++;
		if(d[i] == 0)az++;
		if(d[i] > 0)ap++;
	}

	if(an == n && az == 0 && ap == 0) *ds = -2;
	else if(an == 0 && az == n && ap == 0) *ds = 0;
	else if(an == 0 && az == 0 && ap == n) *ds = 2;
	else if(an > 0 && az > 0 && ap == 0) *ds = -1;
	else if(an == 0 && az > 0 && ap > 0) *ds = 1;
	else if(an > 0 && az >= 0 && ap > 0) *ds = 3;

	ierr = VecRestoreArray(D,&d); CHKERRQ(ierr);
	ierr = VecDestroy(&D); CHKERRQ(ierr);
	return 0;
}

// counts the number of nonzeros on the diagonal
PetscErrorCode DiagonalNonZeros(Mat M, PetscInt* nzd){  
	PetscErrorCode  ierr;
	PetscInt m,n,i,nnz;
	Vec D;

	ierr = Dimension(M,&m,&n);
	PetscScalar* d;

	ierr = VecCreate(PETSC_COMM_WORLD,&D);CHKERRQ(ierr);
	ierr = VecSetSizes(D,PETSC_DECIDE,n);CHKERRQ(ierr);
	ierr = VecSetFromOptions(D);CHKERRQ(ierr);

	ierr = MatGetDiagonal(M,D); CHKERRQ(ierr);
	ierr = VecGetArray(D,&d); CHKERRQ(ierr);
	nnz=0;
	for (i=0; i<n; i++){
		if(d[i] != 0)nnz++;
	}

	*nzd = nnz;

	ierr = VecRestoreArray(D,&d); CHKERRQ(ierr);
	ierr = VecDestroy(&D); CHKERRQ(ierr);
	return 0;
}

// finds the lower bandwidth of a matrix
PetscErrorCode lowerBandwidth(Mat M, PetscInt *lowerb)
{
	PetscInt m,n,i,j,nc,lb;
	PetscErrorCode ierr;  

	ierr = Dimension(M, &m, &n);CHKERRQ(ierr);
	lb = 0;

	for (i=0; i<m; i++) {
		const PetscInt *cols[n];
		const PetscScalar *vals[n];
		ierr = MatGetRow(M,i,&nc,cols,vals);CHKERRQ(ierr);
		if(nc != 0){      
			for(j=0;j<nc;j++){                   
				if(*(vals[0]+j) != 0){
					if((i - *(cols[0]+j)) > lb ){
						lb = i - *(cols[0]+j);
					}
				} 
			}
		}
		ierr = MatRestoreRow(M,i,&nc,cols,vals);CHKERRQ(ierr);
	}
	*lowerb = lb;
	return(0);
}

// finds the upper bandwidth of a matrix
PetscErrorCode upperBandwidth(Mat M, PetscInt *lowerb)
{
	PetscInt m,n,i,j,nc,lb;
	PetscErrorCode ierr;  

	ierr = Dimension(M, &m, &n);CHKERRQ(ierr);
	lb = 0;

	for (i=0; i<m; i++) {
		const PetscInt *cols[n];
		const PetscScalar *vals[n];
		ierr = MatGetRow(M,i,&nc,cols,vals);CHKERRQ(ierr);
		if(nc != 0){      
			for(j=0;j<nc;j++){                   
				if(*(vals[0]+j) != 0){
					if((*(cols[0]+j)-i) > lb ){
						lb = *(cols[0]+j)-i;
					}
				} 
			}
		}
		ierr = MatRestoreRow(M,i,&nc,cols,vals);CHKERRQ(ierr);
	}
	*lowerb = lb;
	return(0);
}
