
/* Program usage:  mpiexec ex1 [-help] [all PETSc options] */

static char help[] = "Solves a linear system with various combinations of KSP + PC.\n\n";

#include <petscksp.h>
#include <petsctime.h>

#undef __FUNCT__
#define __FUNCT__ "main"
int main(int argc,char **args)
{
  Vec            x, b, u;      /* approx solution, RHS, exact solution */
  Mat            A;        /* linear system matrix */
  KSP            ksp;          /* linear solver context */
  PetscErrorCode ierr;
  PetscInt       its[41], i = 1;
  PetscMPIInt    size;
  PetscReal      norm[41];
  PetscLogDouble tsetup[41],tsetup1,tsetup2,tsolve[41],tsolve1,tsolve2;
  char           settings[41][30];
  PetscBool      flg, print_result = PETSC_TRUE;
  
  PetscViewer    fd;              /* viewer */
  char           file[PETSC_MAX_PATH_LEN];

  PetscInitialize(&argc,&args,(char *)0,help);
  ierr = MPI_Comm_size(PETSC_COMM_WORLD,&size);CHKERRQ(ierr);

  ierr = PetscOptionsGetString(PETSC_NULL,"-f",file,PETSC_MAX_PATH_LEN,&flg);CHKERRQ(ierr);
  if (!flg) {
    SETERRQ(PETSC_COMM_WORLD,1,"Must indicate matrix file with the -f option");
  }

  /* Read petsc binary matrix file */
  ierr = PetscViewerBinaryOpen(PETSC_COMM_WORLD,file,FILE_MODE_READ,&fd);CHKERRQ(ierr);
  /*
     Load the matrix and vector; then destroy the viewer.
  */
  ierr = MatCreate(PETSC_COMM_WORLD,&A);CHKERRQ(ierr);
  ierr = MatSetFromOptions(A);CHKERRQ(ierr);
  ierr = MatLoad(A,fd);CHKERRQ(ierr);
  ierr = VecCreate(PETSC_COMM_WORLD,&b);CHKERRQ(ierr);
  ierr = VecSetFromOptions(b);CHKERRQ(ierr);
  ierr = VecLoad(b,fd);CHKERRQ(ierr);
  ierr = PetscViewerDestroy(&fd);CHKERRQ(ierr);

  ierr = MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);

  ierr = VecDuplicate(b,&x);CHKERRQ(ierr);
  ierr = VecDuplicate(b,&u);CHKERRQ(ierr);


  ierr = KSPCreate(PETSC_COMM_WORLD,&ksp);CHKERRQ(ierr); 
  ierr = KSPSetOperators(ksp,A,A,SAME_NONZERO_PATTERN);CHKERRQ(ierr);
  ierr = KSPSetInitialGuessNonzero(ksp,PETSC_FALSE);CHKERRQ(ierr);

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);

  ierr = KSPSetFromOptions(ksp);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
  ierr = KSPSetUpOnBlocks(ksp);CHKERRQ(ierr);

  ierr = PetscTime(&tsetup2);CHKERRQ(ierr);
  tsetup[i] = tsetup2 - tsetup1;

  ierr = PetscTime(&tsolve1);CHKERRQ(ierr);
  ierr = KSPSolve(ksp,b,x);CHKERRQ(ierr); 
  ierr = KSPGetIterationNumber(ksp,&its[i]);CHKERRQ(ierr);
  ierr = MatMult(A,x,u);CHKERRQ(ierr);
  ierr = VecAXPY(u,-1.0,b);CHKERRQ(ierr);
  ierr = VecNorm(u,NORM_2,&norm[i]);CHKERRQ(ierr);
  ierr = PetscTime(&tsolve2);CHKERRQ(ierr);
  tsolve[i] = tsolve2 - tsolve1;

  //strcpy(settings[i], " KSPCG     + PCILU(0)");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," :: Time: %es | Norm: %11G | Iterations: %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

  /* 
     Free work space.  All PETSc objects should be destroyed when they
     are no longer needed.
  */


  ierr = VecDestroy(&x);CHKERRQ(ierr);
  ierr = VecDestroy(&b);CHKERRQ(ierr); 
  ierr = MatDestroy(&A);CHKERRQ(ierr);
  ierr = KSPDestroy(&ksp);CHKERRQ(ierr);

  /*
     Always call PetscFinalize() before exiting a program.  This routine
       - finalizes the PETSc libraries as well as MPI
       - provides summary and diagnostic information if certain runtime
         options are chosen (e.g., -log_summary).
  */

  ierr = PetscFinalize();
  return 0;
}
