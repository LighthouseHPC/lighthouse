static char help[] = "Solves a linear system in parallel with various combinations of KSP + PC.\n\
  -f <input_file> : petsc binary matrix file\n\n";

#include <petscksp.h>
#include <petsctime.h>

extern PetscErrorCode MyKSPMonitor(KSP,PetscInt,PetscReal,void*);

#undef __FUNCT__
#define __FUNCT__ "main"
int main(int argc,char **args)
{
  Mat            A;        /* linear system matrix */
  PetscErrorCode ierr;
  PetscMPIInt    rank;
  PetscBool      flg;
  PetscViewer    fd;         /* viewer */
  char           file[PETSC_MAX_PATH_LEN];

  PetscLogDouble solveTime,endTime,startTime;
  PetscInt       its;
  PetscReal      norm;
  KSP            ksp; // Linear solver context
  Vec            b,x,u; // RHS, solution, vector for norm calculation

  PetscInitialize(&argc,&args,(char *)0,help);
  ierr = MPI_Comm_rank(PETSC_COMM_WORLD,&rank);CHKERRQ(ierr);

  ierr = PetscOptionsGetString(PETSC_NULL,"-f",file,PETSC_MAX_PATH_LEN,&flg);CHKERRQ(ierr);
  if (!flg) {
    SETERRQ(PETSC_COMM_WORLD,1,"Must indicate matrix file with the -f option");
  }
  /* Read file */
  ierr = PetscViewerBinaryOpen(PETSC_COMM_WORLD,file,FILE_MODE_READ,&fd);CHKERRQ(ierr);
  // Create matrix
  ierr = MatCreate(PETSC_COMM_WORLD,&A);CHKERRQ(ierr);
  ierr = MatSetFromOptions(A);CHKERRQ(ierr);
  // Load matrix from file
  ierr = MatLoad(A,fd);CHKERRQ(ierr);
  // Load RHS vector
  ierr = VecCreate(PETSC_COMM_WORLD,&b);CHKERRQ(ierr);
  ierr = VecSetFromOptions(b);CHKERRQ(ierr);
  ierr = VecLoad(b,fd);CHKERRQ(ierr);
  // Destroy viewer
  ierr = PetscViewerDestroy(&fd);CHKERRQ(ierr);
  // Assemble matrix
  ierr = MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  // Create vectors x and u
  ierr = VecDuplicate(b,&x);CHKERRQ(ierr);
  ierr = VecDuplicate(b,&u);CHKERRQ(ierr);
  // Create KSP
  ierr = KSPCreate(PETSC_COMM_WORLD,&ksp); CHKERRQ(ierr);
  ierr = KSPSetInitialGuessNonzero(ksp,PETSC_FALSE);CHKERRQ(ierr);
  ierr = KSPSetOperators(ksp,A,A,SAME_NONZERO_PATTERN);CHKERRQ(ierr);
  ierr = KSPSetFromOptions(ksp); CHKERRQ(ierr);
  // Setup KSP
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
  ierr = KSPSetUpOnBlocks(ksp);CHKERRQ(ierr);
  // Get start time
  ierr = PetscTime(&startTime);CHKERRQ(ierr);
  // Get KSP and PC type
  KSPType kt;
  ierr = KSPGetType(ksp,&kt);
  PC pc;
  ierr = KSPGetPC(ksp,&pc);
  PCType pt;
  ierr = PCGetType(pc,&pt);
  // Print method info
  ierr = PetscPrintf(PETSC_COMM_WORLD,"%s | %s",kt,pt);CHKERRQ(ierr);
  // Make sure the program doesn't crash 
  // while trying to solve the system
  PetscPushErrorHandler(PetscIgnoreErrorHandler,NULL);
  ierr = KSPSolve(ksp,b,x);
  PetscPopErrorHandler();
  // Check if anything went wrong
  if(ierr == 0 || ierr == -1){ 
    // If no error occurred or stopped by MyKSPMonitor, 
    // compute normal and stuff
    ierr = KSPGetIterationNumber(ksp,&its);CHKERRQ(ierr);
    ierr = MatMult(A,x,u);CHKERRQ(ierr);
    ierr = VecAXPY(u,-1.0,b);CHKERRQ(ierr);
    ierr = VecNorm(u,NORM_2,&norm);CHKERRQ(ierr);
    ierr = PetscTime(&endTime);CHKERRQ(ierr);
    // Compute solve time
    solveTime = endTime - startTime;
    // Check if KSP converged
    KSPConvergedReason reason;
    KSPGetConvergedReason(ksp,&reason);
    // Print convergence code, solve time, preconditioned norm, iterations
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %D | %e | %G | %D\n",reason,solveTime,norm,its);CHKERRQ(ierr);
  }
  else{
    // Disaster happened, bail out
    return 0;
  }
  // Again, destroy KSP and vector
  ierr = KSPDestroy(&ksp);CHKERRQ(ierr);
  ierr = VecDestroy(&x);CHKERRQ(ierr);
  ierr = VecDestroy(&b);CHKERRQ(ierr);
  ierr = VecDestroy(&u);CHKERRQ(ierr);  
  return 0;
}