static char help[] = "Solves a linear system with various combinations of KSP + PC.\n\
  -f <input_file> : petsc binary file containing matrix\n\n";

#include <petscksp.h>
#include <petsctime.h>

extern PetscErrorCode MyKSPMonitor(KSP,PetscInt,PetscReal,void*);
extern PetscErrorCode Solve(KSPType,PCType,PetscInt,PetscBool,Mat);

PetscLogDouble lowestTime = 9.0e+99, startTime = 0;
PetscReal lowestNorm = 9.0e+99, pNorm;

PetscInt matRows = 0;
PetscBool hasConverged = PETSC_FALSE;

#undef __FUNCT__
#define __FUNCT__ "main"
int main(int argc,char **args)
{
  Vec            d;      /* vector for fixing missing diagonal entries */
  Mat            A;       /* linear system matrix */
  
  PetscErrorCode ierr;
  PetscMPIInt    size;
  PetscBool      flg, pcSideLeft=PETSC_TRUE,isSymmetric=PETSC_FALSE;

  char           filein[PETSC_MAX_PATH_LEN],buf[PETSC_MAX_PATH_LEN],link[PETSC_MAX_PATH_LEN];
  char*          lnk;
  FILE*          file; 
  PetscInt       k,m,n,nnz,col,row;
  PetscScalar    val;

  // Initialize Petsc
  PetscInitialize(&argc,&args,(char *)0,help);
  ierr = MPI_Comm_size(PETSC_COMM_WORLD,&size);CHKERRQ(ierr);
  if (size != 1) SETERRQ(PETSC_COMM_WORLD,1,"This is a uniprocessor program only!");

  ierr = PetscOptionsGetString(PETSC_NULL,"-f",filein,PETSC_MAX_PATH_LEN,&flg);CHKERRQ(ierr);
  if (!flg) {
    SETERRQ(PETSC_COMM_WORLD,1,"Must indicate matrix file with the -f option");
  }

  // Open Matrix Market File for reading
  ierr = PetscFOpen(PETSC_COMM_SELF,filein,"r",&file);CHKERRQ(ierr);

  // Check if matrix is symmetric
  do {
    fgets(buf,PETSC_MAX_PATH_LEN-1,file);
    if(strstr(buf, "symmetric") != NULL){
      isSymmetric = PETSC_TRUE;
    }
    if(strstr(buf, "http")){
      strcpy(link,buf);
      lnk = link;
    }
  }while (buf[0] == '%');

  // Get matrix size
  sscanf(buf,"%d %d %d\n",&m,&n,&nnz);
  // Set global variable for row
  matRows = m;

  // Create vector d for fixing missing diagonal entries
  ierr = VecCreate(PETSC_COMM_WORLD,&d);CHKERRQ(ierr);
  ierr = VecSetSizes(d,PETSC_DECIDE,n);CHKERRQ(ierr);
  ierr = VecSetFromOptions(d);CHKERRQ(ierr);
  
  // Set all entries to 0
  ierr = VecSet(d,0);CHKERRQ(ierr);

  // Set matrix format
  ierr = MatCreateSeqAIJ(PETSC_COMM_WORLD,m,n,nnz*2/m,0,&A);CHKERRQ(ierr);
  ierr = MatSetOption(A,MAT_NEW_NONZERO_ALLOCATION_ERR,PETSC_FALSE);CHKERRQ(ierr); 

  // Read matrix market file and build petsc matrix 
  for (k=0; k<nnz; k++) {
    fscanf(file,"%d %d %le\n",&row,&col,(double*)&val);
    row = row-1; col = col-1 ;
    ierr = MatSetValues(A,1,&row,1,&col,&val,INSERT_VALUES);CHKERRQ(ierr);
    // If matrix is symmetric,
    // insert same entry on the other side of the diagonal
    if(isSymmetric){ 
      if (row != col){
        ierr = MatSetValues(A,1,&col,1,&row,&val,INSERT_VALUES);CHKERRQ(ierr);
      }
    }
    // Matrix has diagonal entry, 
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

  // Finish builing matrix
  ierr = MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);

  // Print link to the matrix
  PetscPrintf(PETSC_COMM_WORLD,"%s",lnk);

  // Solve the linear system with the following KSP+PC combinations

  if(isSymmetric){ // If matrix is symmetric, try this order  

    // Apply preconditioner from left
    pcSideLeft = PETSC_TRUE;

    Solve(KSPCG,PCILU,0,pcSideLeft,A);
    Solve(KSPCG,PCILU,1,pcSideLeft,A);
    Solve(KSPCG,PCILU,2,pcSideLeft,A);
    Solve(KSPCG,PCJACOBI,-1,pcSideLeft,A);
    Solve(KSPCG,PCSOR,-1,pcSideLeft,A);
    Solve(KSPCG,PCNONE,-1,pcSideLeft,A);

    Solve(KSPCGS,PCILU,0,pcSideLeft,A);
    Solve(KSPCGS,PCILU,1,pcSideLeft,A);
    Solve(KSPCGS,PCILU,2,pcSideLeft,A);
    Solve(KSPCGS,PCJACOBI,-1,pcSideLeft,A);
    Solve(KSPCGS,PCSOR,-1,pcSideLeft,A);
    Solve(KSPCGS,PCNONE,-1,pcSideLeft,A);

    Solve(KSPTFQMR,PCILU,0,pcSideLeft,A);
    Solve(KSPTFQMR,PCILU,1,pcSideLeft,A);
    Solve(KSPTFQMR,PCILU,2,pcSideLeft,A);
    Solve(KSPTFQMR,PCJACOBI,-1,pcSideLeft,A);
    Solve(KSPTFQMR,PCSOR,-1,pcSideLeft,A);
    Solve(KSPTFQMR,PCNONE,-1,pcSideLeft,A);

    Solve(KSPBICG,PCILU,0,pcSideLeft,A);
    Solve(KSPBICG,PCILU,1,pcSideLeft,A);
    Solve(KSPBICG,PCILU,2,pcSideLeft,A);
    Solve(KSPBICG,PCJACOBI,-1,pcSideLeft,A);
    Solve(KSPBICG,PCNONE,-1,pcSideLeft,A);

    Solve(KSPBCGS,PCILU,0,pcSideLeft,A);
    Solve(KSPBCGS,PCILU,1,pcSideLeft,A);
    Solve(KSPBCGS,PCILU,2,pcSideLeft,A);
    Solve(KSPBCGS,PCJACOBI,-1,pcSideLeft,A);
    Solve(KSPBCGS,PCSOR,-1,pcSideLeft,A);
    Solve(KSPBCGS,PCNONE,-1,pcSideLeft,A);

    Solve(KSPGMRES,PCILU,0,pcSideLeft,A);
    Solve(KSPGMRES,PCILU,1,pcSideLeft,A);
    Solve(KSPGMRES,PCILU,2,pcSideLeft,A);
    Solve(KSPGMRES,PCJACOBI,-1,pcSideLeft,A);
    Solve(KSPGMRES,PCSOR,-1,pcSideLeft,A);
    Solve(KSPGMRES,PCNONE,-1,pcSideLeft,A);

    // FGMRES supports preconditioning from right side only
    // So set pcSideLeft to false
    pcSideLeft = PETSC_FALSE;

    Solve(KSPFGMRES,PCILU,0,pcSideLeft,A);
    Solve(KSPFGMRES,PCILU,1,pcSideLeft,A);
    Solve(KSPFGMRES,PCILU,2,pcSideLeft,A);
    Solve(KSPFGMRES,PCJACOBI,-1,pcSideLeft,A);
    Solve(KSPFGMRES,PCSOR,-1,pcSideLeft,A);
    Solve(KSPFGMRES,PCNONE,-1,pcSideLeft,A);

  }else{ // If matrix is unsymmetric, try this order  

    pcSideLeft = PETSC_TRUE;

    Solve(KSPBCGS,PCILU,0,pcSideLeft,A);
    Solve(KSPBCGS,PCILU,1,pcSideLeft,A);
    Solve(KSPBCGS,PCILU,2,pcSideLeft,A);
    Solve(KSPBCGS,PCJACOBI,-1,pcSideLeft,A);
    Solve(KSPBCGS,PCSOR,-1,pcSideLeft,A);
    Solve(KSPBCGS,PCNONE,-1,pcSideLeft,A);

    Solve(KSPGMRES,PCILU,0,pcSideLeft,A);
    Solve(KSPGMRES,PCILU,1,pcSideLeft,A);
    Solve(KSPGMRES,PCILU,2,pcSideLeft,A);
    Solve(KSPGMRES,PCJACOBI,-1,pcSideLeft,A);
    Solve(KSPGMRES,PCSOR,-1,pcSideLeft,A);
    Solve(KSPGMRES,PCNONE,-1,pcSideLeft,A);

    // FGMRES supports preconditioning from right side only
    // So set pcSideLeft to false
    pcSideLeft = PETSC_FALSE;

    Solve(KSPFGMRES,PCILU,0,pcSideLeft,A);
    Solve(KSPFGMRES,PCILU,1,pcSideLeft,A);
    Solve(KSPFGMRES,PCILU,2,pcSideLeft,A);
    Solve(KSPFGMRES,PCJACOBI,-1,pcSideLeft,A);
    Solve(KSPFGMRES,PCSOR,-1,pcSideLeft,A);
    Solve(KSPFGMRES,PCNONE,-1,pcSideLeft,A);

    pcSideLeft = PETSC_TRUE;

    Solve(KSPBICG,PCILU,0,pcSideLeft,A);
    Solve(KSPBICG,PCILU,1,pcSideLeft,A);
    Solve(KSPBICG,PCILU,2,pcSideLeft,A);
    Solve(KSPBICG,PCJACOBI,-1,pcSideLeft,A);
    Solve(KSPBICG,PCNONE,-1,pcSideLeft,A);

    Solve(KSPTFQMR,PCILU,0,pcSideLeft,A);
    Solve(KSPTFQMR,PCILU,1,pcSideLeft,A);
    Solve(KSPTFQMR,PCILU,2,pcSideLeft,A);
    Solve(KSPTFQMR,PCJACOBI,-1,pcSideLeft,A);
    Solve(KSPTFQMR,PCSOR,-1,pcSideLeft,A);
    Solve(KSPTFQMR,PCNONE,-1,pcSideLeft,A);

    Solve(KSPCGS,PCILU,0,pcSideLeft,A);
    Solve(KSPCGS,PCILU,1,pcSideLeft,A);
    Solve(KSPCGS,PCILU,2,pcSideLeft,A);
    Solve(KSPCGS,PCJACOBI,-1,pcSideLeft,A);
    Solve(KSPCGS,PCSOR,-1,pcSideLeft,A);
    Solve(KSPCGS,PCNONE,-1,pcSideLeft,A);

    Solve(KSPCG,PCILU,0,pcSideLeft,A);
    Solve(KSPCG,PCILU,1,pcSideLeft,A);
    Solve(KSPCG,PCILU,2,pcSideLeft,A);
    Solve(KSPCG,PCJACOBI,-1,pcSideLeft,A);
    Solve(KSPCG,PCSOR,-1,pcSideLeft,A);
    Solve(KSPCG,PCNONE,-1,pcSideLeft,A);  
  }
  //PetscPrintf(PETSC_COMM_WORLD,"L time: %e | L norm: %G \n",lowestTime,lowestNorm);

  // Destroy Matrix and Vector
  ierr = VecDestroy(&d);CHKERRQ(ierr);  
  ierr = MatDestroy(&A);CHKERRQ(ierr);
  ierr = PetscFinalize();
  return 0;
}

PetscErrorCode Solve(KSPType kt, PCType pt, PetscInt pcFactorLevel, PetscBool pcSideLeft, Mat A){
  
  PetscLogDouble solveTime,endTime;
  PetscInt       its;
  PetscReal      norm;
  PetscErrorCode ierr;
  KSP            ksp; // Linear solver context
  PC             pc; // Preconditioner
  Vec            b,x,u; // RHS, solution, vector for norm calculation

  // Create RHS vector
  ierr = VecCreate(PETSC_COMM_WORLD,&b);CHKERRQ(ierr);
  ierr = VecSetSizes(b,PETSC_DECIDE,matRows);CHKERRQ(ierr);
  ierr = VecSetFromOptions(b);CHKERRQ(ierr);
  ierr = VecSet(b,1);CHKERRQ(ierr);

  // Create vectors x and u
  ierr = VecDuplicate(b,&x);CHKERRQ(ierr);
  ierr = VecDuplicate(b,&u);CHKERRQ(ierr);

  // Create KSP
  ierr = KSPCreate(PETSC_COMM_WORLD,&ksp); CHKERRQ(ierr);
  ierr = KSPSetFromOptions(ksp); CHKERRQ(ierr);
  // Setup KSP monitor
  ierr = KSPMonitorSet(ksp,MyKSPMonitor,NULL,0); CHKERRQ(ierr);

  ierr = KSPSetOperators(ksp,A,A,SAME_NONZERO_PATTERN);CHKERRQ(ierr);
  ierr = KSPSetInitialGuessNonzero(ksp,PETSC_FALSE);CHKERRQ(ierr);
  // Set KSP type
  ierr = KSPSetType(ksp,kt);CHKERRQ(ierr);
  // Set which side PC will be applied from
  if(pcSideLeft == PETSC_FALSE){
    ierr = KSPSetPCSide(ksp,PC_RIGHT);CHKERRQ(ierr);
  }
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  // Set PC type
  ierr = PCSetType(pc,pt);CHKERRQ(ierr);
  if(pcFactorLevel != -1){
    ierr = PCFactorSetLevels(pc,pcFactorLevel);CHKERRQ(ierr);
  }
  // Setup KSP
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);

  ierr = PetscTime(&startTime);CHKERRQ(ierr);
  // Make sure the program doesn't crash
  PetscPushErrorHandler(PetscIgnoreErrorHandler,NULL);
  ierr = KSPSolve(ksp,b,x);
  PetscPopErrorHandler();
  // Check if anything went wrong
  if(ierr == 0 || ierr == -1){ 
    // If no error occurred or stopped by MyKSPMonitor, compute normal and stuff
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
    if(reason > 0){
      hasConverged = PETSC_TRUE;
      if(solveTime < lowestTime){
        lowestTime = solveTime;
      }
      if(pNorm < lowestNorm){
        lowestNorm = pNorm;
      }
    }
    // Print method info
    if(pcFactorLevel != -1){
      ierr = PetscPrintf(PETSC_COMM_WORLD,"%s | %s(%D)",kt,pt,pcFactorLevel);CHKERRQ(ierr);
    }
    else{ 
      ierr = PetscPrintf(PETSC_COMM_WORLD,"%s | %s",kt,pt);CHKERRQ(ierr);
    }
    // Print convergence code, solve time, preconditioned norm, iterations
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %D | %e | %G | %D\n",reason,solveTime,pNorm,its);CHKERRQ(ierr);

  }
  else{ // Disaster happened, print error code
    if(pcFactorLevel != -1){
      ierr = PetscPrintf(PETSC_COMM_WORLD,"%s | %s(%D) | Failed | Errorcode %D ||\n",kt,pt,pcFactorLevel,ierr);CHKERRQ(ierr);    
    }else{
      ierr = PetscPrintf(PETSC_COMM_WORLD,"%s | %s | Failed | Errorcode %D ||\n",kt,pt,ierr);CHKERRQ(ierr);    
    }
  }

  // Destroy KSP and vector
  ierr = KSPDestroy(&ksp);CHKERRQ(ierr);
  ierr = VecDestroy(&x);CHKERRQ(ierr);
  ierr = VecDestroy(&b);CHKERRQ(ierr);
  ierr = VecDestroy(&u);CHKERRQ(ierr);  
  return 0;
}

PetscErrorCode MyKSPMonitor(KSP ksp,PetscInt n,PetscReal rnorm,void *dummy)
{
  PetscLogDouble currentTime = 0;
  PetscTime(&currentTime);
  //PetscPrintf(PETSC_COMM_WORLD,"It: %D, norm: %G\n",n,rnorm);
  pNorm = rnorm;
  if(hasConverged){
    if(currentTime - startTime > (lowestTime*10)){
      if(rnorm > lowestNorm){
        SETERRQ(PETSC_COMM_WORLD,-1,"This is taking way too long!\n");
      }
    }
  }
  return 0;
}