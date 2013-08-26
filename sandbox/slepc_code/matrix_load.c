
/* Program usage:  mpiexec ex1 [-help] [all PETSc options] */

static char help[] = "Uploads matrix in binary for parallel use.\n\n";

/*T
   Main operation: Uploads matrix in petsc binary format for parallel use
   Input file format: Matrix market format (matrix and rhs in the same file)
   Processor: 1 (sequential)
T*/

#include <petsctime.h>
#include "petscsys.h" 
#include <petscmat.h>

#undef __FUNCT__
#define __FUNCT__ "main"
int main(int argc,char **args)
{
  Mat            A;
  Vec            b;
  char           filein[PETSC_MAX_PATH_LEN],buf[PETSC_MAX_PATH_LEN];
  PetscInt       i,m,n,nnz,col,row;
  PetscErrorCode ierr;
  PetscMPIInt    size;
  double 	 re, im;
  PetscScalar    val;
  FILE*          file;
  PetscRandom    r;
  char           fileout[PETSC_MAX_PATH_LEN];
  PetscBool	 isComplex=0;


  
PetscInitialize(&argc,&args,(char *)0,help);
//SlepcInitialize(&argc,&args,(char*)0,help);

  ierr = MPI_Comm_size(PETSC_COMM_WORLD,&size);CHKERRQ(ierr);
  if (size > 1) SETERRQ(PETSC_COMM_WORLD,1,"Uniprocessor Example only\n");

  /* Read in matrix and RHS */
  ierr = PetscOptionsGetString(PETSC_NULL,"-fin",filein,PETSC_MAX_PATH_LEN,PETSC_NULL);CHKERRQ(ierr);
  ierr = PetscOptionsGetString(PETSC_NULL,"-fout",fileout,PETSC_MAX_PATH_LEN,PETSC_NULL);CHKERRQ(ierr);
  ierr = PetscOptionsGetBool(PETSC_NULL,"-c",&isComplex,PETSC_NULL);CHKERRQ(ierr);
  ierr = PetscFOpen(PETSC_COMM_SELF,filein,"r",&file);CHKERRQ(ierr);

  /* process header with comments */
  do fgets(buf,PETSC_MAX_PATH_LEN-1,file);
  while (buf[0] == '%');

  /* The first non-comment line has the matrix dimensions */
  sscanf(buf,"%d %d %d\n",&m,&n,&nnz);
  //printf ("m = %d, n = %d, nnz = %d\n",m,n,nnz);

  ierr = MatCreateSeqAIJ(PETSC_COMM_WORLD,m,n,nnz*2/m,0,&A);CHKERRQ(ierr);
  ierr = MatSetOption(A,MAT_NEW_NONZERO_ALLOCATION_ERR,PETSC_FALSE);CHKERRQ(ierr);
  ierr = VecCreate(PETSC_COMM_WORLD,&b);CHKERRQ(ierr);
  ierr = VecSetSizes(b,PETSC_DECIDE,n);CHKERRQ(ierr);
  ierr = VecSetFromOptions(b);CHKERRQ(ierr);
  ierr = PetscRandomCreate(PETSC_COMM_SELF,&r);CHKERRQ(ierr);
  ierr = PetscRandomSetFromOptions(r);CHKERRQ(ierr);
  ierr = VecSetRandom(b,r);CHKERRQ(ierr);
  if(isComplex==0)
  {
	  for (i=0; i<nnz; i++) {
	    fscanf(file,"%d %d %le\n",&row,&col,(double*)&val);
	    row = row-1; col = col-1 ;
	    ierr = MatSetValues(A,1,&row,1,&col,&val,INSERT_VALUES);CHKERRQ(ierr);
	    if (row != col) {
	      ierr = MatSetValues(A,1,&col,1,&row,&val,INSERT_VALUES);CHKERRQ(ierr);
	    }
	  }
  }
  else
  {
	  for (i=0; i<nnz; i++) {
	    fscanf(file,"%d %d %le %le\n",&row,&col,(double*)&re, (double*)&im);
            val = re + im * PETSC_i;
	    row = row-1; col = col-1 ;
	    ierr = MatSetValues(A,1,&row,1,&col,&val,INSERT_VALUES);CHKERRQ(ierr);
	    if (row != col) {
	      ierr = MatSetValues(A,1,&col,1,&row,&val,INSERT_VALUES);CHKERRQ(ierr);
	    }
	  }
  }
  
  fclose(file);

  ierr = MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);

  ierr = PetscPrintf(PETSC_COMM_SELF,"Reading matrix completes.\n");CHKERRQ(ierr);

  PetscViewer viewer;

  ierr = PetscViewerBinaryOpen(PETSC_COMM_WORLD,fileout,FILE_MODE_WRITE,&viewer);CHKERRQ(ierr);
  ierr = MatView(A, viewer);CHKERRQ(ierr);
  ierr = PetscViewerDestroy(&viewer);CHKERRQ(ierr);

  
//Destructors
  ierr = MatDestroy(&A);CHKERRQ(ierr);
  ierr = PetscFinalize();CHKERRQ(ierr);
  return 0;
}
