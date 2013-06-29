static char help[] = "Computes various properties of a real square matrix.";

#include <petscmat.h>
extern PetscErrorCode Dimension(Mat,PetscInt*,PetscInt*);
extern PetscErrorCode BlockSize(Mat,PetscInt*);
extern PetscErrorCode Nonzeros(Mat,PetscInt*);
extern PetscErrorCode MaxNonzerosPerRow(Mat,PetscInt*);
extern PetscErrorCode MinNonzerosPerRow(Mat,PetscInt*);
extern PetscErrorCode DummyRows(Mat,PetscInt*);
extern PetscErrorCode LeftBandwidth(Mat,PetscInt*);
extern PetscErrorCode RightBandwidth(Mat,PetscInt*);
extern PetscErrorCode NumericValueSymmetry(Mat,PetscBool*);
extern PetscErrorCode NonZeroPatternSymmetry(Mat,PetscBool*);
extern PetscErrorCode Trace(Mat,PetscScalar*);
extern PetscErrorCode AbsoluteTrace(Mat,PetscScalar*);
extern PetscErrorCode OneNorm(Mat,PetscScalar*);
extern PetscErrorCode InfinityNorm(Mat,PetscScalar*);
extern PetscErrorCode FrobeniusNorm(Mat,PetscScalar*);
extern PetscErrorCode RowDiagonalDominance(Mat,PetscInt*); 
extern PetscErrorCode ColumnDiagonalDominance(Mat,PetscInt*); 
extern PetscErrorCode RowVariance(Mat,Vec);

#undef __FUNCT__
#define __FUNCT__ "main"
int main(int argc,char **args)
{
  Mat            A,B;
  char           filein[PETSC_MAX_PATH_LEN],buf[PETSC_MAX_PATH_LEN];
  PetscInt       i,m,n,nnz,col,row;
  PetscErrorCode ierr;
  PetscMPIInt    size;
  PetscScalar    val,one = 1;
  FILE*          file;

  PetscInitialize(&argc,&args,(char *)0,help);

  ierr = MPI_Comm_size(PETSC_COMM_WORLD,&size);CHKERRQ(ierr);
  if (size > 1) SETERRQ(PETSC_COMM_WORLD,1,"Uniprocessor Example only\n");

  /* Read in matrix and RHS */
  ierr = PetscOptionsGetString(PETSC_NULL,"-f",filein,PETSC_MAX_PATH_LEN,PETSC_NULL);CHKERRQ(ierr);
  ierr = PetscFOpen(PETSC_COMM_SELF,filein,"r",&file);CHKERRQ(ierr);

  /* process header with comments */
  char sym[] = "symmetric";
  PetscBool isSymmetric = PETSC_FALSE;
  do {
    fgets(buf,PETSC_MAX_PATH_LEN-1,file);
    if(strstr(buf, sym) != NULL){
      isSymmetric = PETSC_TRUE;
    }
  }while (buf[0] == '%');

  /* The first non-comment line has the matrix dimensions */
  sscanf(buf,"%d %d %d\n",&m,&n,&nnz);
  //printf ("m = %d, n = %d, nnz = %d\n",m,n,nnz);

  if(isSymmetric){
    ierr = MatCreateSeqAIJ(PETSC_COMM_WORLD,m,n,nnz*2/m,0,&A);CHKERRQ(ierr);  
    ierr = MatCreateSeqAIJ(PETSC_COMM_WORLD,m,n,nnz*2/m,0,&B);CHKERRQ(ierr);  
  }else{
    ierr = MatCreateSeqAIJ(PETSC_COMM_WORLD,m,n,nnz/m,0,&A);CHKERRQ(ierr);  
    ierr = MatCreateSeqAIJ(PETSC_COMM_WORLD,m,n,nnz/m,0,&B);CHKERRQ(ierr);  
  }
  
  ierr = MatSetOption(A,MAT_NEW_NONZERO_ALLOCATION_ERR,PETSC_FALSE);CHKERRQ(ierr);
  ierr = MatSetOption(B,MAT_NEW_NONZERO_ALLOCATION_ERR,PETSC_FALSE);CHKERRQ(ierr);

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
  }

  fclose(file);

  ierr = MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyBegin(B,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(B,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);

  ierr = PetscPrintf(PETSC_COMM_SELF,"Matrix conversion complete.\n");CHKERRQ(ierr);
  
  /*------------------------------------*/ 

  PetscInt r=0,c=0,nz=0;
  PetscScalar x;
  PetscBool s;
  Vec V;

  ierr = Dimension(A,&r,&c);CHKERRQ(ierr);
  printf ("Dimension: m = %d, n = %d\n",r,c);

  ierr = BlockSize(A, &r);
  printf ("Block size: %d\n",r);

  ierr = Nonzeros(A,&nz);CHKERRQ(ierr);
  printf ("Nonzeros: %d\n",nz);

  MaxNonzerosPerRow(A,&nz);
  printf ("Max. nonzeros per row: %d\n",nz);

  MinNonzerosPerRow(A,&nz);
  printf ("Min. nonzeros per row: %d\n",nz);

  DummyRows(A,&nz);
  printf ("Number of dummy rows: %d\n",nz);

  ierr = NumericValueSymmetry(A,&s);CHKERRQ(ierr);
  if(s)printf ("Numerically Symmetric: True\n");
  else printf ("Numerically Symmetric: False\n");

  ierr = NonZeroPatternSymmetry(B,&s);CHKERRQ(ierr);
  if(s)printf ("Structurally Symmetric: True\n");
  else printf ("Structurally Symmetric: False\n");

  Trace(A,&x);
  printf ("Trace: %G\n",x);

  AbsoluteTrace(A,&x);
  printf ("Absolute trace: %G\n",x);

  RowDiagonalDominance(A,&r); 
  printf ("Row Diagonal Dominance: %d\n",r);

  ColumnDiagonalDominance(A,&r); 
  printf ("Column Diagonal Dominance: %d\n",r);

  ierr = VecCreate(PETSC_COMM_WORLD,&V);CHKERRQ(ierr);
  ierr = VecSetSizes(V,PETSC_DECIDE,n);CHKERRQ(ierr);
  ierr = VecSetFromOptions(V);CHKERRQ(ierr);
  ierr = VecSet(V,0);CHKERRQ(ierr);

  RowVariance(A,V); 
  printf ("Row variance:");
  VecView(V,PETSC_VIEWER_STDOUT_WORLD);



  /*------------------------------------*/ 

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
  PetscErrorCode ierr;
  MatInfo mi;
  ierr = MatGetInfo(M,MAT_GLOBAL_SUM,&mi);CHKERRQ(ierr);
  *nonzeros = (PetscInt)mi.nz_used;
  return(0);
}

// finds the maximum number of nonzeros per row
PetscErrorCode MaxNonzerosPerRow(Mat M, PetscInt *maxNz)
{
  PetscErrorCode ierr;
  PetscInt m, n, i, nz;
  const PetscInt *cols[n];
  const PetscScalar *vals[n];

  *maxNz = 0;
  ierr = Dimension(M, &m, &n);CHKERRQ(ierr);

  for(i = 0; i < m; i++){
    ierr = MatGetRow(M,i,&nz,cols,vals);CHKERRQ(ierr);
    if(&nz != NULL){
      if(nz > *maxNz){
        *maxNz = nz;
      }  
    }    
    ierr = MatRestoreRow(M,i,&nz,cols,vals);CHKERRQ(ierr);
  }

  return(0);
}

// finds the minimum number of nonzeros per row
PetscErrorCode MinNonzerosPerRow(Mat M, PetscInt *minNz)
{
  PetscErrorCode ierr;
  PetscInt m, n, i, nz;
  const PetscInt *cols[n];
  const PetscScalar *vals[n];
  
  ierr = Dimension(M, &m, &n);CHKERRQ(ierr);
  *minNz = n;

  for(i = 0; i < m; i++){
    ierr = MatGetRow(M,i,&nz,cols,vals);CHKERRQ(ierr);
    if(&nz != NULL){
      if(nz < *minNz){
        *minNz = nz;
      }  
    }    
    ierr = MatRestoreRow(M,i,&nz,cols,vals);CHKERRQ(ierr);
  }
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
    if(&nz != NULL){
      if(nz == 1){
        *dummyRows += 1;
      }  
    }
    ierr = MatRestoreRow(M,i,&nz,cols,vals);CHKERRQ(ierr);
  }
  return(0);
}

PetscErrorCode LeftBandwidth(Mat M, PetscInt *maxNz)
{
  return(0);
}

PetscErrorCode RightBandwidth(Mat M, PetscInt *maxNz)
{
  return(0);
}

// checks the numerical symmetry, if symmetric returns true
PetscErrorCode NumericValueSymmetry(Mat A, PetscBool *s)
{
  PetscErrorCode ierr;
  Mat Atrans;
  ierr = MatTranspose(A, MAT_INITIAL_MATRIX,&Atrans);CHKERRQ(ierr);
  ierr = MatEqual(A, Atrans, s);CHKERRQ(ierr);
  ierr = MatDestroy(&Atrans);CHKERRQ(ierr);
  return(0);
}

// checks the nonzero pattern symmetry, if symmetric returns true
PetscErrorCode NonZeroPatternSymmetry(Mat B, PetscBool *s)
{
  PetscErrorCode ierr;
  Mat Btrans;
  ierr = MatTranspose(B, MAT_INITIAL_MATRIX,&Btrans);CHKERRQ(ierr);
  ierr = MatEqual(B, Btrans, s);CHKERRQ(ierr);
  ierr = MatDestroy(&Btrans);CHKERRQ(ierr);
  return(0);
}

// computes trace (sum of diagonal elements)
PetscErrorCode Trace(Mat M, PetscScalar *trace){
  PetscErrorCode ierr;
  ierr = MatGetTrace(M,trace);
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
  ierr = MatTranspose(M,MAT_INITIAL_MATRIX,&T);
  RowDiagonalDominance(T,dom);
  return 0;
}

// computes the row variance of a matrix

PetscErrorCode RowVariance(Mat M, Vec V){  
  PetscScalar ssum = 0,dv[1],mean = 0,var = 0; 
  PetscInt m,n,i,j,nc;
  PetscErrorCode ierr;
  Vec rowSum;
  
  ierr = Dimension(M, &m, &n);CHKERRQ(ierr);
  ierr = VecCreate(PETSC_COMM_WORLD,&rowSum);CHKERRQ(ierr);
  ierr = VecSetSizes(rowSum,PETSC_DECIDE,n);CHKERRQ(ierr);
  ierr = VecSetFromOptions(rowSum);CHKERRQ(ierr);
  ierr = MatGetRowSum(M, rowSum);CHKERRQ(ierr);
  
  for (i=0; i<m; i++){
    ierr = VecGetValues(rowSum,1,&i,dv);CHKERRQ(ierr);
    const PetscInt *cols[n];
    const PetscScalar *vals[n];
    mean = dv[0]/n;
    //printf ("Rowsum: %g\n",dv[0]);
    ierr = MatGetRow(M,i,&nc,cols,vals);CHKERRQ(ierr);
    if(nc != 0){
      for(j=0;j<nc;j++){
        ssum = ssum + (*(vals[0]+j)-mean)*(*(vals[0]+j)-mean);
      }
    }else{
      ssum = 0;
    }

    var = ssum/n;
    ierr = VecSetValue(V,i,var,ADD_VALUES);
    ierr = MatRestoreRow(M,i,&nc,cols,vals);CHKERRQ(ierr);
  }
  VecAssemblyBegin(V); VecAssemblyEnd(V);
  return 0;
}

// computes the column variance of a matrix

PetscErrorCode ColumnVariance(Mat M, Vec V){  
  PetscErrorCode  ierr;
  Mat T;
  ierr = MatTranspose(M,MAT_INITIAL_MATRIX,&T);
  RowVariance(T,V);
  return 0;
}