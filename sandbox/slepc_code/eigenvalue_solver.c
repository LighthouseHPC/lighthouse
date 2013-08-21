
/* Program usage:  mpiexec ex1 [-help] [all PETSc options] */

static char help[] = "Solves a eigenvalue system.\n\n";

/*T
   Main operation: Solve a linear system
   Input file format: PETSc binary format (matrix and rhs in the same file)
   Processor: 1 (sequential)
   Output format: PETSc binary format
T*/

//#include <petscksp.h>
#include <petsctime.h>
#include "petscsys.h" 
#include <slepceps.h>

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
  PetscScalar    val;
  FILE*          file;
  //PetscViewer    view;
  PetscRandom    r;
  PetscLogDouble numberOfFlops, tsolve1, tsolve2;
EPS            eps;         /* eigenproblem solver context */
  const EPSType  type;
  PetscReal      error,tol,re,im;
  PetscScalar    kr,ki;//,value[3];
  Vec            xr=0,xi=0;
  PetscInt       nev,maxit,its,nconv;
  EPSWhich 	 which;
  //PetscBool      FirstBlock=PETSC_FALSE,LastBlock=PETSC_FALSE;

  
//PetscInitialize(&argc,&args,(char *)0,help);
SlepcInitialize(&argc,&args,(char*)0,help);

  ierr = MPI_Comm_size(PETSC_COMM_WORLD,&size);CHKERRQ(ierr);
  if (size > 1) SETERRQ(PETSC_COMM_WORLD,1,"Uniprocessor Example only\n");

  /* Read in matrix and RHS */
  ierr = PetscOptionsGetString(PETSC_NULL,"-fin",filein,PETSC_MAX_PATH_LEN,PETSC_NULL);CHKERRQ(ierr);
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

  for (i=0; i<nnz; i++) {
    fscanf(file,"%d %d %le\n",&row,&col,(double*)&val);
    row = row-1; col = col-1 ;
    ierr = MatSetValues(A,1,&row,1,&col,&val,INSERT_VALUES);CHKERRQ(ierr);
    if (row != col) {
      ierr = MatSetValues(A,1,&col,1,&row,&val,INSERT_VALUES);CHKERRQ(ierr);
    }
  }
  fclose(file);

  ierr = MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);

  //ierr = PetscPrintf(PETSC_COMM_SELF,"Reading matrix completes.\n");CHKERRQ(ierr);



/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                Create the eigensolver and set various options
     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
  /* 
     Create eigensolver context
  */
  ierr = EPSCreate(PETSC_COMM_WORLD,&eps);CHKERRQ(ierr);
  /* 
     Set operators. In this case, it is a standard eigenvalue problem
  */
  ierr = EPSSetOperators(eps,A,PETSC_NULL);CHKERRQ(ierr);
  //ierr = EPSSetProblemType(eps,EPS_HEP);CHKERRQ(ierr);

  /*
     Set solver parameters at runtime
  */
  ierr = EPSSetFromOptions(eps);CHKERRQ(ierr);

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                      Solve the eigensystem
     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
  PetscTime(tsolve1);
  ierr = EPSSolve(eps);CHKERRQ(ierr);
  PetscTime(tsolve2);
  /*
     Optional: Get some information from the solver and display it
  */
  ierr = EPSGetWhichEigenpairs(eps, &which);CHKERRQ(ierr);
  ierr = EPSGetDimensions(eps,&nev,PETSC_NULL,PETSC_NULL);CHKERRQ(ierr);
  ierr = EPSGetType(eps,&type);CHKERRQ(ierr);
  ierr = EPSGetTolerances(eps,&tol,&maxit);CHKERRQ(ierr);
  ierr = EPSGetConverged(eps,&nconv);CHKERRQ(ierr);
  ierr = EPSGetIterationNumber(eps,&its);CHKERRQ(ierr);
  ierr = PetscGetFlops(&numberOfFlops);CHKERRQ(ierr);
  
  //Print output:
  ierr = PetscPrintf(PETSC_COMM_WORLD,"%D\t %D\t %.4G\t %s\t %D\t %D\t %F\t %2.1e\t",which, nev, tol, type, nconv, its, numberOfFlops, (tsolve2-tsolve1));CHKERRQ(ierr);

  if (nconv>0) {
    for (i=0;i<nconv;i++) {
      /* 
        Get converged eigenpairs: i-th eigenvalue is stored in kr (real part) and
        ki (imaginary part)
      */
      ierr = EPSGetEigenpair(eps,i,&kr,&ki,xr,xi);CHKERRQ(ierr);
      /*
         Compute the relative error associated to each eigenpair
      */
      ierr = EPSComputeRelativeError(eps,i,&error);CHKERRQ(ierr);

#if defined(PETSC_USE_COMPLEX)
      re = PetscRealPart(kr);
      im = PetscImaginaryPart(kr);
#else
      re = kr;
      im = ki;
#endif 
      if (im!=0.0) {
    //    ierr = PetscPrintf(PETSC_COMM_WORLD," %9F%+9F j %12G\n",re,im,error);CHKERRQ(ierr);
      } else {
    //    ierr = PetscPrintf(PETSC_COMM_WORLD,"   %12F       %12G\n",re,error);CHKERRQ(ierr); 
      }
      ierr = PetscPrintf(PETSC_COMM_WORLD,"%12G\t", error);CHKERRQ(ierr);
    }
    
  }

ierr = PetscPrintf(PETSC_COMM_WORLD,"\n");CHKERRQ(ierr);
  
//Destructors
  ierr = MatDestroy(&A);CHKERRQ(ierr);
  //ierr = PetscFinalize();
ierr = SlepcFinalize();CHKERRQ(ierr);
  return 0;
}
