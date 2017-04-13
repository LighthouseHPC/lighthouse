static char help[] = "Solves a linear system in parallel with various combinations of KSP + PC.\n\
  -f <input_file> : petsc binary matrix file\n\n";

#include <sys/stat.h>
#include <fcntl.h>
#include <libgen.h>
#include "petscksp.h"
#include "petsctime.h"
#include "petscsys.h"


extern PetscErrorCode MyKSPMonitor(KSP,PetscInt,PetscReal,void*);

#undef __FUNCT__
#define __FUNCT__ "main"
int main(int argc,char **args)
{
  Mat            A;        /* linear system matrix */
  PetscErrorCode ierr;
  PetscMPIInt    rank=0, nprocs=1;
  PetscBool      flg;
  PetscViewer    fd;         /* viewer */
  PetscViewer    log;
  char           file[PETSC_MAX_PATH_LEN];
  char           logfile[PETSC_MAX_PATH_LEN];
  char           lockfile[PETSC_MAX_PATH_LEN], tmpstr[PETSC_MAX_PATH_LEN], dirname[PETSC_MAX_PATH_LEN], matrix[PETSC_MAX_PATH_LEN];
  char           hash[20];

  PetscLogDouble solveTime,endTime,startTime;
  PetscInt       its;
  PetscReal      norm;
  KSP            ksp; // Linear solver context
  Vec            b,x,u; // RHS, solution, vector for norm calculation
  PetscScalar    one = 1.0;
  PetscInt	 m, n, i;
  FILE           *lock;

/*
  if (rank == 0) {
    printf("Command line arguments:\n");
    for (i=0; i < argc; i++) 
      printf("%d: %s\n", i, args[i]);
  }
  // Save args
  int argcount = argc;
  char **argv = (char**) malloc (argc*sizeof(char*));
  for (i=0; i < argc; i++) {
    argv[i] = (char*) malloc(strlen(args[i]) + 1);
    strcpy(argv[i],args[i]);
  }
  MPI_Comm_rank(MPI_COMM_WORLD,&rank);
*/
  PetscInitialize(&argc,&args,(char *)0,help);
  ierr = MPI_Comm_rank(PETSC_COMM_WORLD,&rank);CHKERRQ(ierr);
  ierr = MPI_Comm_size(PETSC_COMM_WORLD,&nprocs); CHKERRQ(ierr);

  ierr = PetscOptionsGetString(PETSC_NULL, PETSC_NULL, "-hash",hash,PETSC_MAX_PATH_LEN,&flg);CHKERRQ(ierr);
  if (!flg) {
    strcpy(hash,"nohash");
  }

  ierr = PetscOptionsGetString(PETSC_NULL, PETSC_NULL,"-f",file,PETSC_MAX_PATH_LEN,&flg);CHKERRQ(ierr);
  if (!flg) {
    PetscPrintf(PETSC_COMM_WORLD,"Must indicate matrix file with the -f option");
  }
  ierr = PetscOptionsGetString(PETSC_NULL, PETSC_NULL,"-logfile",logfile,PETSC_MAX_PATH_LEN,&flg);CHKERRQ(ierr);
  if (!flg) {
    PetscPrintf(PETSC_COMM_WORLD,"Must indicate log file name with -logfile option");
  }
  /* Read file */
  ierr = PetscViewerBinaryOpen(PETSC_COMM_WORLD,file,FILE_MODE_READ,&fd);CHKERRQ(ierr);
  // Create matrix
  ierr = MatCreate(PETSC_COMM_WORLD,&A);CHKERRQ(ierr);
  //ierr = MatSetType(A,MATMPIAIJ); CHKERRQ(ierr);
  ierr = MatSetFromOptions(A);CHKERRQ(ierr);
  // Load matrix from file
  ierr = MatLoad(A,fd);CHKERRQ(ierr);
  ierr = PetscViewerDestroy(&fd);CHKERRQ(ierr);
  ierr = MatGetSize(A, &m, &n); CHKERRQ(ierr);
  // Assemble matrix
  //ierr = MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  //ierr = MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);

  // Create RHS vector
  ierr = VecCreate(PETSC_COMM_WORLD,&b);CHKERRQ(ierr);
  ierr = VecSetSizes(b,PETSC_DECIDE,n); CHKERRQ(ierr);
  ierr = VecSetFromOptions(b);CHKERRQ(ierr);
  ierr = VecSet(b,one);  CHKERRQ(ierr);
  //ierr = VecLoad(b,fd);CHKERRQ(ierr);
  // Create vectors x and u
  ierr = VecDuplicate(b,&x);CHKERRQ(ierr);
  ierr = VecDuplicate(b,&u);CHKERRQ(ierr);

  // Create KSP
  ierr = KSPCreate(PETSC_COMM_WORLD,&ksp); CHKERRQ(ierr);
  ierr = KSPSetInitialGuessNonzero(ksp,PETSC_FALSE);CHKERRQ(ierr);
  ierr = KSPSetOperators(ksp,A,A);CHKERRQ(ierr);
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
  //log = PETSC_VIEWER_STDOUT_WORLD;

  PetscLogDefaultBegin(); 
  // Make sure the program doesn't crash 
  // while trying to solve the system
  PetscPushErrorHandler(PetscIgnoreErrorHandler,NULL);
  ierr = KSPSolve(ksp,b,x);
  PetscPopErrorHandler();
  // Check if anything went wrong
  KSPConvergedReason reason;
  if(ierr == 0 || ierr == -1){ 
    // If no error occurred or stopped by MyKSPMonitor, 
    // compute normal and stuff
    ierr = KSPGetIterationNumber(ksp,&its);CHKERRQ(ierr);
    ierr = MatMult(A,x,u);CHKERRQ(ierr);
    ierr = VecAXPY(u,-1.0,b);CHKERRQ(ierr);
    ierr = VecNorm(u,NORM_2,&norm);CHKERRQ(ierr);
    KSPGetConvergedReason(ksp,&reason);
  } else {
    reason = -99; 
    its = -99;
  }
  ierr = PetscTime(&endTime);CHKERRQ(ierr);
  // Compute solve time
  solveTime = endTime - startTime;
  // Check if KSP converged
  // Print convergence code, solve time, preconditioned norm, iterations
//  if (rank == 0) {
    ierr = PetscViewerASCIIOpen(PETSC_COMM_WORLD, logfile, &log); CHKERRQ(ierr);
    ierr = PetscViewerASCIIPrintf(log, "Hash: %s\n", hash);
    ierr = PetscViewerASCIIPrintf(log, "%s | %s | reason=%D | time=%e | norm=%g | its=%D | p=%d\n",kt,pt,reason,solveTime,norm,its,nprocs);CHKERRQ(ierr);
    ierr = KSPView(ksp,log);
    ierr = PCView(pc,log);
    ierr = PetscLogView(log);
    ierr = PetscViewerDestroy(&log);CHKERRQ(ierr);
 // }
  
  // Again, destroy KSP and vector
  ierr = KSPDestroy(&ksp);CHKERRQ(ierr);
  ierr = VecDestroy(&x);CHKERRQ(ierr);
  ierr = VecDestroy(&b);CHKERRQ(ierr);
  ierr = VecDestroy(&u);CHKERRQ(ierr);  

  //if (rank == 0) remove(lockfile);
  PetscFinalize();
  return 0;
}
