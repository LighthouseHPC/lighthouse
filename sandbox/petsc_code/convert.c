
static char help[] = "Reads in a matrix in MatrixMarket format. Writes\n\
it using the PETSc sparse format. It also adds a Vector set to 1 to the\n\
output file. Input parameters are:\n\
  -fin <filename> : input file\n\
  -fout <filename> : output file\n\n";

#include <petscmat.h>

#undef __FUNCT__
#define __FUNCT__ "main"
int main(int argc,char **args)
{
  Mat            A;
  Vec            b, d;
  char           filein[PETSC_MAX_PATH_LEN],fileout[PETSC_MAX_PATH_LEN],buf[PETSC_MAX_PATH_LEN];
  PetscInt       i,m,n,k,nnz,col,row;
  PetscErrorCode ierr;
  PetscMPIInt    size;
  PetscScalar    val;
  PetscBool      flg, isSymmetric = PETSC_FALSE;
  FILE*          file;
  PetscViewer    view;

  // Initialize PETSc
  PetscInitialize(&argc,&args,(char *)0,help);
  ierr = MPI_Comm_size(PETSC_COMM_WORLD,&size);CHKERRQ(ierr);
  if (size != 1) SETERRQ(PETSC_COMM_WORLD,1,"This is a uniprocessor program only!");

  /* Read in matrix and RHS */
  ierr = PetscOptionsGetString(PETSC_NULL,"-fin",filein,PETSC_MAX_PATH_LEN,&flg);CHKERRQ(ierr);
  if (!flg) {
    SETERRQ(PETSC_COMM_WORLD,1,"Must indicate input matrix file with the -fin option");
  }
  ierr = PetscFOpen(PETSC_COMM_SELF,filein,"r",&file);CHKERRQ(ierr);

  /* process header with comments */

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
    // For symmetric matrix allocate nnz*2/m nonzeros per row 
    ierr = MatCreateSeqAIJ(PETSC_COMM_WORLD,m,n,nnz*2/m,0,&A);CHKERRQ(ierr);  
  }
  else{
    // For unsymmetric matrix allocate nnz/m nonzeros per row 
    ierr = MatCreateSeqAIJ(PETSC_COMM_WORLD,m,n,nnz/m,0,&A);CHKERRQ(ierr);
  }
  ierr = MatSetOption(A,MAT_SYMMETRIC,isSymmetric);CHKERRQ(ierr);
  ierr = MatSetOption(A,MAT_NEW_NONZERO_ALLOCATION_ERR,PETSC_FALSE);CHKERRQ(ierr);
  // Create RHS vector, b
  ierr = VecCreate(PETSC_COMM_WORLD,&b);CHKERRQ(ierr);
  ierr = VecSetSizes(b,PETSC_DECIDE,n);CHKERRQ(ierr);
  ierr = VecSetFromOptions(b);CHKERRQ(ierr);
  // Set all entires of vector b to 1
  ierr = VecSet(b,1);CHKERRQ(ierr);
  // We'll use vector d for putting 0s at the missing diagonal entries of matrix A
  ierr = VecDuplicate(b,&d);CHKERRQ(ierr);

  // Read matrix market file and insert data into petsc matrix
  for (i=0; i<nnz; i++) {
    fscanf(file,"%d %d %le\n",&row,&col,(double*)&val);
    row = row-1; col = col-1 ;
    ierr = MatSetValues(A,1,&row,1,&col,&val,INSERT_VALUES);CHKERRQ(ierr);
    if(isSymmetric){
      if (row != col){
        // Matrix is symmetric, insert same value at the transpose location
        ierr = MatSetValues(A,1,&col,1,&row,&val,INSERT_VALUES);CHKERRQ(ierr);
      }
    }
    // If matrix A has a diagonal entry, set the entry of vector d
    // to 1 at that location
    if(row == col)
       VecSetValue(d,row,1,ADD_VALUES);
  }

  VecAssemblyBegin(d); VecAssemblyEnd(d);
  PetscScalar dv[1];
  val = 0;
  // Now check if vector d has any 0 entries
  // If it does than insert a 0 at that location of the matrix A
  for(k=0; k<m; k++){
    VecGetValues(d,1,&k,dv);
    if(dv[0] == 0)
      ierr = MatSetValues(A,1,&k,1,&k,&val,INSERT_VALUES);CHKERRQ(ierr);
  }
  // We're done reading the file, close it
  fclose(file);
  // Assemble matrix
  ierr = MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);

  // Prepare for writing the matrix and rhs vector to file
  ierr = PetscOptionsGetString(PETSC_NULL,"-fout",fileout,PETSC_MAX_PATH_LEN,PETSC_NULL);CHKERRQ(ierr);
  ierr = PetscViewerBinaryOpen(PETSC_COMM_WORLD,fileout,FILE_MODE_WRITE,&view);CHKERRQ(ierr);
  ierr = MatView(A,view);CHKERRQ(ierr);
  ierr = VecView(b,view);CHKERRQ(ierr);
  // Destroy file writer
  ierr = PetscViewerDestroy(&view);CHKERRQ(ierr);
  // Destroy matrix and vectors
  ierr = VecDestroy(&b);CHKERRQ(ierr);
  ierr = VecDestroy(&d);CHKERRQ(ierr);
  ierr = MatDestroy(&A);CHKERRQ(ierr);
  // One last thing
  ierr = PetscFinalize();
  return 0;
}
