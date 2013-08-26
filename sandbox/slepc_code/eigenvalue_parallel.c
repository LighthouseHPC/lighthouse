
/* Program usage:  mpiexec ex1 [-help] [all PETSc options] */

static char help[] = "Solves a eigenvalue system in parallel.\n\n";

/*T
   Main operation: Solve a eigenvalue problem in parallel
   Input file format: Matrix market format (matrix and rhs in the same file)
   Processor: 1 (sequential)
T*/

#include <petsctime.h>
#include "petscsys.h" 
#include <slepceps.h>

#undef __FUNCT__
#define __FUNCT__ "main"
int main(int argc,char **args)
{
  Mat            A;
  PetscInt       i;
  PetscErrorCode ierr;
  char           file[PETSC_MAX_PATH_LEN];
  PetscLogDouble numberOfFlops, tsolve1, tsolve2;
EPS            eps;         /* eigenproblem solver context */
  const EPSType  type;
  PetscReal      error,tol,re,im;
  PetscScalar    kr,ki;
  Vec            xr=0,xi=0;
  PetscInt       nev,maxit,its,nconv;
  EPSWhich 	 which;
  EPSProblemType problemType;
  PetscMPIInt    rank;
  PetscMPIInt    numberOfProcessors;
  PetscBool      flg;
  PetscBool      isComplex;
  PetscViewer    fd; 

  SlepcInitialize(&argc,&args,(char*)0,help);

  ierr = MPI_Comm_rank(PETSC_COMM_WORLD,&rank);CHKERRQ(ierr);
  ierr = MPI_Comm_size(PETSC_COMM_WORLD,&numberOfProcessors);CHKERRQ(ierr);

  ierr = PetscOptionsGetString(PETSC_NULL,"-fin",file,PETSC_MAX_PATH_LEN,&flg);CHKERRQ(ierr);
  if (!flg) {
    SETERRQ(PETSC_COMM_WORLD,1,"Must indicate matrix file with the -fin option");
  }
  /* Read file */
  ierr = PetscViewerBinaryOpen(PETSC_COMM_WORLD,file,FILE_MODE_READ,&fd);CHKERRQ(ierr);
  // Create matrix
  ierr = MatCreate(PETSC_COMM_WORLD,&A);CHKERRQ(ierr);
  ierr = MatSetFromOptions(A);CHKERRQ(ierr);
  // Load matrix from file
  ierr = MatLoad(A,fd);CHKERRQ(ierr);
  
  // Destroy viewer
  ierr = PetscViewerDestroy(&fd);CHKERRQ(ierr);
  // Assemble matrix
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
  ierr = EPSGetProblemType(eps, &problemType);CHKERRQ(ierr);
  ierr = EPSGetWhichEigenpairs(eps, &which);CHKERRQ(ierr);
  ierr = EPSGetDimensions(eps,&nev,PETSC_NULL,PETSC_NULL);CHKERRQ(ierr);
  ierr = EPSGetType(eps,&type);CHKERRQ(ierr);
  ierr = EPSGetTolerances(eps,&tol,&maxit);CHKERRQ(ierr);
  ierr = EPSGetConverged(eps,&nconv);CHKERRQ(ierr);
  ierr = EPSGetIterationNumber(eps,&its);CHKERRQ(ierr);
  ierr = PetscGetFlops(&numberOfFlops);CHKERRQ(ierr);

#if defined(PETSC_USE_COMPLEX)
      isComplex = 1;
#else
      isComplex = 0;
#endif 
  
  //Print output:
  ierr = PetscPrintf(PETSC_COMM_WORLD,"%D\t %D\t %D\t %D\t %D\t %.4G\t %s\t %D\t %D\t %F\t %2.1e\t",isComplex, numberOfProcessors, problemType, which, nev, tol, type, nconv, its, numberOfFlops, (tsolve2-tsolve1));CHKERRQ(ierr);

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
