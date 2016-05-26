static char help[] = "Computes 32 features of moose matrices .\n\
  -f <input_file> : petsc binary matrix file\n\n";
  //To run : ./properties_moose -f data/mat_files/twophasestress_1_1_1.mat 


#include <petscksp.h>
#include <petsctime.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <libgen.h>
#include <petscmat.h>
#include <unistd.h>

PetscErrorCode Dimension(Mat,PetscInt*,PetscInt*);
PetscErrorCode BlockSize(Mat,PetscInt*);
PetscErrorCode Nonzeros(Mat, PetscInt *);
PetscErrorCode NonzerosQ_(Mat,PetscInt*);
PetscErrorCode MaxNonzerosPerRow(Mat,PetscInt*);
PetscErrorCode MinNonzerosPerRow(Mat,PetscInt*);
PetscErrorCode AvgNonzerosPerRow(Mat,PetscInt*);
PetscErrorCode DummyRows(Mat,PetscInt*);
PetscErrorCode DummyRowsKind(Mat,PetscInt*);
PetscErrorCode AbsoluteNonZeroSum(Mat,PetscScalar*);  
PetscErrorCode NumericValueSymmetryV1(Mat,PetscInt*);
PetscErrorCode NonZeroPatternSymmetryV1(Mat,PetscInt*);
PetscErrorCode NumericValueSymmetryV2(Mat,PetscScalar*);
PetscErrorCode NonZeroPatternSymmetryV2(Mat,PetscScalar*);
PetscErrorCode Trace(Mat,PetscScalar*);
PetscErrorCode AbsoluteTrace(Mat,PetscScalar*);
PetscErrorCode OneNorm(Mat,PetscScalar*);
PetscErrorCode InfinityNorm(Mat,PetscScalar*);
PetscErrorCode FrobeniusNorm(Mat,PetscScalar*);
PetscErrorCode SymmetricInfinityNorm(Mat,PetscScalar*);
PetscErrorCode SymmetricFrobeniusNorm(Mat,PetscScalar*);
PetscErrorCode AntiSymmetricInfinityNorm(Mat,PetscScalar*);
PetscErrorCode AntiSymmetricFrobeniusNorm(Mat,PetscScalar*);
PetscErrorCode RowDiagonalDominance(Mat,PetscInt*); 
PetscErrorCode ColumnDiagonalDominance(Mat,PetscInt*); 
PetscErrorCode RowVariance(Mat,PetscScalar*);
PetscErrorCode ColumnVariance(Mat,PetscScalar*);
PetscErrorCode DiagonalAverage(Mat,PetscScalar*);
PetscErrorCode DiagonalVariance(Mat,PetscScalar*);
PetscErrorCode DiagonalSign(Mat,PetscInt*);
PetscErrorCode DiagonalNonZeros(Mat,PetscInt*);
PetscErrorCode lowerBandwidth(Mat,PetscInt*);
PetscErrorCode upperBandwidth(Mat,PetscInt*);
PetscErrorCode MatIsSymmetric(Mat,PetscReal,PetscBool *);
PetscErrorCode rowAndColLogSpread(Mat,PetscScalar *,PetscScalar *);


int file_exists (char *filename) {
  struct stat   buffer;   
  return (stat (filename, &buffer) == 0);
}

#undef __FUNCT__
#define __FUNCT__ "main"
int main(int argc,char **args)
{
  Mat            A,B;        /* linear system matrix */
  PetscErrorCode ierr;
  PetscBool      flg, hflg, lflg;
  PetscViewer    fd;         /* viewer */
  PetscViewer    logviewer;    /* output log file (optional) */
  char           file[PETSC_MAX_PATH_LEN];
  char           logfile[PETSC_MAX_PATH_LEN];
  PetscInt	 m, n;
  PetscBool      isSymmetric;
  char           buf[300], header[500];
  int            len = 0, rank;
  
  PetscInitialize(&argc,&args,(char *)0,help);
  MPI_Comm_rank(PETSC_COMM_WORLD,&rank);
  if (rank == 0) sprintf(header, "%s", "MinNNZperRow, RowVariance, ColVariance, DiagVariance, Nonzeros, Dimension, FrobeniusNorm, AntiSymmetricFrobeniusNorm, OneNorm, InfinityNorm, SymmetricInfinityNorm, AntiSymmetricInfinityNorm, MaxNNZperRow, Trace, AbsTrace, MinNNZperRow, AvgNNZperRow, DummyRows, DummyRowsKind, NumericValueSymmetry1, NNZPatternSymmetry1, NumericValueSymmetry2, NNZPatternSymmetry2, RowDiagDominance, ColDiagDominancy, DiagAverage, DiagSign, DiagNNZ, LowerBW, UpperBW, RowLogValSpread, ColLogValSpread, Symmetric, Name");

  PetscOptionsHasName(NULL,NULL, "-header", &hflg);
  if (hflg) {
     PetscPrintf(PETSC_COMM_WORLD, "%s\n", header);
  }

  ierr = PetscOptionsGetString(PETSC_NULL,PETSC_NULL,"-f",file,PETSC_MAX_PATH_LEN,&flg);CHKERRQ(ierr);
  if (!flg) {
    PetscPrintf(PETSC_COMM_WORLD,"Must indicate matrix file with the -f option");
    PetscFinalize();
    exit(1);
  }
  ierr = PetscOptionsGetString(PETSC_NULL,PETSC_NULL,"-logfile",logfile,PETSC_MAX_PATH_LEN,&lflg);CHKERRQ(ierr);
  /* Read file */
  ierr = PetscViewerBinaryOpen(PETSC_COMM_WORLD,file,FILE_MODE_READ,&fd);CHKERRQ(ierr);
  // Create matrix
  ierr = MatCreate(PETSC_COMM_WORLD,&A);CHKERRQ(ierr);
  ierr = MatSetFromOptions(A);CHKERRQ(ierr);
  // Load matrix from file
  ierr = MatLoad(A,fd);CHKERRQ(ierr);
  PetscBool doView = PETSC_TRUE;
  PetscViewer    viewer;
  if (doView) {
		PetscViewerASCIIOpen(PETSC_COMM_WORLD, NULL, &viewer);
		PetscViewerPushFormat(viewer, PETSC_VIEWER_ASCII_INFO);
		}
  ierr = PetscViewerDestroy(&fd);CHKERRQ(ierr);
  ierr = MatGetSize(A, &m, &n); CHKERRQ(ierr);

  // Make sure the program doesn't crash while trying to solve the system
  PetscPushErrorHandler(PetscIgnoreErrorHandler,NULL);
  PetscPopErrorHandler();
    
/* Compute and print matrix properties */ 
  PetscInt r=0,c=0,nz=0;
  PetscScalar x, rv, cv;
  
  MinNonzerosPerRow(A,&nz); //printing Min. nonzeros per row: 1
  len += sprintf(buf + len, "%d, ", nz);

  RowVariance(A,&x); 
  len += sprintf (buf + len, "%G, ",x);
  
  ColumnVariance(A,&x); 
  len += sprintf (buf + len, "%G, ",x);
  
  DiagonalVariance(A,&x); 
  len += sprintf (buf + len, "%g, ",x);
  
  Nonzeros(A,&nz);
  len += sprintf (buf + len, "%d, ",nz);
  
  Dimension(A,&r,&c); //rows, columns
  len += sprintf (buf + len, "%d, ",r);
  len += sprintf (buf + len, "%d, ",c);

  FrobeniusNorm(A,&x);
  len += sprintf (buf + len, "%G, ",x);

  SymmetricFrobeniusNorm(A,&x);
  len += sprintf (buf + len, "%G, ",x);

  AntiSymmetricFrobeniusNorm(A,&x);
  len += sprintf (buf + len, "%G, ",x);

  OneNorm(A,&x);
  len += sprintf (buf + len, "%G, ",x);

  InfinityNorm(A,&x);
  len += sprintf (buf + len, "%G, ",x);

  SymmetricInfinityNorm(A,&x);
  len += sprintf (buf + len, "%G, ",x);
  
  AntiSymmetricInfinityNorm(A,&x);
  len += sprintf (buf + len, "%G, ",x);
  
  MaxNonzerosPerRow(A,&nz);
  len += sprintf (buf + len, "%G, ",x);
  
  Trace(A,&x);
  len += sprintf (buf + len, "%G, ",x);
  
  AbsoluteTrace(A,&x);
  len += sprintf (buf + len, "%G, ",x);

  MinNonzerosPerRow(A,&nz);
  len += sprintf (buf + len, "%d, ",nz);
  
  AvgNonzerosPerRow(A,&nz);
  len += sprintf (buf + len, "%d, ",nz);
  
  DummyRows(A,&nz);
  len += sprintf (buf + len, "%d, ",nz);
  
  DummyRowsKind(A,&nz);
  len += sprintf (buf + len, "%d, ",nz);
  
  NumericValueSymmetryV1(A,&r);
  len += sprintf (buf + len, "%d, ",r);
  
  NonZeroPatternSymmetryV1(A,&r);
  len += sprintf (buf + len, "%d, ",r);

  NumericValueSymmetryV2(A,&x);
  len += sprintf (buf + len, "%G, ",x);

  NonZeroPatternSymmetryV2(A,&x);
  len += sprintf (buf + len, "%G, ",x);

  RowDiagonalDominance(A,&r); 
  len += sprintf (buf + len, "%d, ",r);
  
  ColumnDiagonalDominance(A,&r); 
  len += sprintf (buf + len, "%d, ",r);
  
  DiagonalAverage(A,&x); 
  len += sprintf (buf + len, "%g, ",x);
  
  DiagonalSign(A,&r); 
  len += sprintf (buf + len, "%d, ",r);

  DiagonalNonZeros(A,&r);
  len += sprintf (buf + len, "%d, ",r);

  lowerBandwidth(A, &r);
  len += sprintf (buf + len, "%d, ",r);

  upperBandwidth(A, &r);
  len += sprintf (buf + len, "%d, ",r);

  ierr = rowAndColLogSpread(A, &rv, &cv); CHKERRQ(ierr);
  len += sprintf (buf + len, "%g, ",rv);
  len += sprintf (buf + len, "%g, ",cv);
   
  ierr = MatIsSymmetric(A,0.0,&isSymmetric); CHKERRQ(ierr);
  len += sprintf (buf + len, "%d, ",isSymmetric);

  char *fname = basename(file); // getting matrix name from the path 
  len += sprintf (buf + len, "%s",fname);

  // Print the properties
  if (lflg) {
     FILE *logfd;
     if (rank == 0) {
        // Open local file for appending
        if( file_exists(logfile) ) {
           logfd = fopen(logfile,"a");
        } else {
           logfd = fopen(logfile, "w+"); 
           fprintf(logfd, "%s\n", header);
        }
        fprintf(logfd, "%s\n", buf);
        fclose(logfd);
     }
  }
  PetscPrintf (PETSC_COMM_WORLD, "%s\n ",buf) ; //matrix name

  ierr = MatDestroy(&A);CHKERRQ(ierr);
  ierr = PetscFinalize();
  return 0;
}

// number of rows and columns
PetscErrorCode Dimension(Mat M, PetscInt *m, PetscInt *n)
{
  PetscErrorCode ierr;
  ierr = MatGetSize(M,m,n);CHKERRQ(ierr);
  return(0);
}

  // finds the minimum number of nonzeros per row
PetscErrorCode MinNonzerosPerRow(Mat M, PetscInt *minNz)
{
  PetscErrorCode ierr;
  PetscInt m, n, i, j, nnz=0, nc=0;
  
  ierr = Dimension(M, &m, &n);CHKERRQ(ierr);
  *minNz = n;

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
    
// computes the row variance of a matrix
PetscErrorCode RowVariance(Mat M, PetscScalar* vrn){  
  PetscScalar ssum,dv[1],mean = 0,var = 0,vecSum = 0; 
  PetscInt m,n,i,j,nc=0;
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
  PetscInt m,n,i,j,nc=0,nnz;
  PetscErrorCode ierr;  

  ierr = Dimension(M, &m, &n);CHKERRQ(ierr);  
  nnz = 0;
  for (i=0; i<m; i++) { //loop over each row
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
  PetscInt m, n, i, j, nnz, nc=0;
  *maxNz = 0;
  ierr = Dimension(M, &m, &n);CHKERRQ(ierr);

//go through all rows
  for(i = 0; i < m; i++){
    const PetscInt *cols[n];
    const PetscScalar *vals[n];
    ierr = MatGetRow(M,i,&nc,cols,vals);CHKERRQ(ierr);
    nnz = 0;
    if(nc != 0){   //go through all columns of each row   
      for(j=0;j<nc;j++){                   
        if(*(vals[0]+j) != 0) nnz++;
      }
    } //after every row keep updating the maxNz
    if(nnz > *maxNz){
      *maxNz = nnz;
    }    
    ierr = MatRestoreRow(M,i,&nc,cols,vals);CHKERRQ(ierr);
  }
  return(0);
}

// finds the average number of nonzeros per row
PetscErrorCode AvgNonzerosPerRow(Mat M, PetscInt *avgNz)
{
  PetscErrorCode ierr;
  PetscInt m, n, i, j, nc=0, nnz=0;
  const PetscInt *cols[n];
  const PetscScalar *vals[n];
  PetscScalar sum=0;
  ierr = Dimension(M, &m, &n);CHKERRQ(ierr);

  for(i = 0; i < m; i++){ //go over each row
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
} //could have also just used nonzeros()/m

// finds the number of rows with only one non-zero element 
// eg : 0 0 0 0 2 0 0 0 0 0 
//      0 -45 0 0 0 0 0 0 0
PetscErrorCode DummyRows(Mat M, PetscInt *dummyRows)
{
  PetscErrorCode ierr;
  PetscInt m, i, nz=0;
  PetscInt n=0;
  const PetscInt *cols[n];
  const PetscScalar *vals[n];
  
  ierr = Dimension(M, &m, &n);CHKERRQ(ierr);
  *dummyRows = 0;

  for(i = 0; i < m; i++){ //go over all rows
    ierr = MatGetRow(M,i,&nz,cols,vals);CHKERRQ(ierr);
    if(nz == 1){
      if(*(vals[0]) != 0) *dummyRows += 1;      
    }
    ierr = MatRestoreRow(M,i,&nz,cols,vals);CHKERRQ(ierr);
  }
  return(0);
}

//3 kinds : 0, 1 , 2 
// 0 if all dummy rows have a one on the diagonal,
// 1 if the value is not one, 2 if not on the diagonal
PetscErrorCode DummyRowsKind(Mat M, PetscInt *dummyRowsKind)
{
  PetscErrorCode ierr;
  PetscInt m, n, i, nz=0;
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
  PetscInt m,n,i,j,nc=0;
  PetscErrorCode ierr;  

  ierr = Dimension(M, &m, &n);//CHKERRQ(ierr);  
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
// if A = A(Transpose) then this returns 1
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
//if A and A(Transpose ) have same nonzero pattern then returns 1
PetscErrorCode NonZeroPatternSymmetryV1(Mat B, PetscInt *s)
{
  PetscErrorCode ierr;
  Mat Btrans;
  PetscBool b;
  *s = 0;
  ierr = MatTranspose(B, MAT_INITIAL_MATRIX,&Btrans);
  ierr = MatEqual(B, Btrans, &b);//CHKERRQ(ierr);
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
  ierr = MatTranspose(B,MAT_INITIAL_MATRIX,&T);
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
  PetscInt m,n,i,j,nc=0, dom0=0, dom1=0, dom2=0;
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

// computes the average of the absolute values of the diagonal elements of a matrix
PetscErrorCode DiagonalAverage(Mat M, PetscScalar* v){  
  PetscErrorCode  ierr;
  PetscScalar da = 0;
  PetscInt m,n;
  ierr = Dimension(M,&m,&n);CHKERRQ(ierr);
  ierr = AbsoluteTrace(M,&da);CHKERRQ(ierr);
  *v = da/m;
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
  ierr = MatGetDiagonal(M,D); CHKERRQ(ierr); // get the diagonals of the matrix
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
  PetscInt m,n,i,j,nc=0,lb;
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
  PetscInt m,n,i,j,nc=0,lb;
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

// computes the variance of the diagonal elements
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


/*!
  Spread of log of values in rows and columns.

  - "row-variability" : \f$\max_i \log_{10} {\max_j|a_{ij}|\over\min_j|a_{ij}|} \f$
  - "col-variability" : \f$\max_j \log_{10} {\max_i|a_{ij}|\over\min_i|a_{ij}|} \f$

  This is a computational routine.
 */
#undef __FUNCT__
#define __FUNCT__ "rowAndColLogSpread"
PetscErrorCode rowAndColLogSpread(Mat A, PetscScalar *rv, PetscScalar *cv)
{
  MPI_Comm comm;
  PetscScalar *rmax,*rmin,*cmax,*cmin,*cmaxx,*cminn,rr_local;
  int m,n,M,N,first,last,i,id,row;
  PetscBool has; PetscErrorCode ierr;
  *rv = 0; *cv = 0;

  PetscFunctionBegin;
  ierr = PetscObjectGetComm((PetscObject)A,&comm); CHKERRQ(ierr);
  ierr = MatGetSize(A,&M,&N); CHKERRQ(ierr);
  ierr = MatGetLocalSize(A,&m,&n); CHKERRQ(ierr);
  ierr = MatGetOwnershipRange(A,&first,&last); CHKERRQ(ierr);
  ierr = PetscMalloc(m*sizeof(PetscReal),&rmax); CHKERRQ(ierr);
  ierr = PetscMalloc(m*sizeof(PetscReal),&rmin); CHKERRQ(ierr);
  ierr = PetscMalloc(N*sizeof(PetscReal),&cmax); CHKERRQ(ierr);
  ierr = PetscMalloc(N*sizeof(PetscReal),&cmin); CHKERRQ(ierr);
  ierr = PetscMalloc(N*sizeof(PetscReal),&cmaxx); CHKERRQ(ierr);
  ierr = PetscMalloc(N*sizeof(PetscReal),&cminn); CHKERRQ(ierr);

  /* init; we take logs, so 1.e+5 is large enough */
  for (i=0; i<m; i++) {rmax[i] = 0; rmin[i] = 1.e+5;}
  for (i=0; i<N; i++) {cmax[i] = 0; cmin[i] = 1.e+5;}
  for (row=first; row<last; row++) {
    int irow=row-first,ncols,icol; const int *cols; const PetscScalar *vals;
    ierr = MatGetRow(A,row,&ncols,&cols,&vals); CHKERRQ(ierr);
    for (icol=0; icol<ncols; icol++) {
      int col=cols[icol];
      if (vals[icol]) {
	PetscReal logval = (PetscReal) log10(PetscAbsScalar(vals[icol]));
	if (logval>rmax[irow]) rmax[irow] = logval;
	if (logval<rmin[irow]) rmin[irow] = logval;
	if (logval>cmax[col]) cmax[col] = logval;
	if (logval<cmin[col]) cmin[col] = logval;
      }
    }
    ierr = MatRestoreRow(A,row,&ncols,&cols,&vals); CHKERRQ(ierr);
  }

  /*
   * now get global row spread
   */
  for (i=0; i<m; i++) rmax[i] -= rmin[i];
  for (i=1; i<m; i++) if (rmax[i]>rmax[0]) rmax[0] = rmax[i];
  rr_local = rmax[0];
  MPI_Allreduce(&rr_local,rv,1,MPIU_SCALAR,MPI_MAX,comm);


  /*
   * global column spread: reduce the full length row of values,
   * then local max is global
   */
  MPI_Allreduce(cmax,cmaxx,N,MPIU_SCALAR,MPI_MAX,comm);
  MPI_Allreduce(cmin,cminn,N,MPIU_SCALAR,MPI_MIN,comm);
  PetscScalar t;
  for (i=0; i<N; i++) {
     t = cmaxx[i] - cminn[i];
     if (t > *cv) *cv = t;
  }

  ierr = PetscFree(rmax); CHKERRQ(ierr);
  ierr = PetscFree(cmax); CHKERRQ(ierr);
  ierr = PetscFree(cmaxx); CHKERRQ(ierr);
  ierr = PetscFree(rmin); CHKERRQ(ierr);
  ierr = PetscFree(cmin); CHKERRQ(ierr);
  ierr = PetscFree(cminn); CHKERRQ(ierr);
  PetscFunctionReturn(0);
}


