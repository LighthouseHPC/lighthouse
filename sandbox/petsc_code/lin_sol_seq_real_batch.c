
/* Program usage:  mpiexec ex1 [-help] [all PETSc options] */

static char help[] = "Solves a real linear system sequentially with various combinations of KSPs and PCs.\n\n";

#include <petscksp.h>
#include <petsctime.h>

#undef __FUNCT__
#define __FUNCT__ "main"
int main(int argc,char **args)
{
  Vec            x, b, u, d;      /* approx solution, RHS, exact solution, vector used for fixing diagonal */
  Mat            A;        /* linear system matrix */
  KSP            ksp;          /* linear solver context */
  PC             pc;
  PetscErrorCode ierr;
  PetscInt       its[41], i = 1;
  PetscMPIInt    size;
  PetscReal      norm[41];
  PetscLogDouble tsetup[41],tsetup1,tsetup2,tsolve[41],tsolve1,tsolve2;
  char           settings[41][30];
  PetscBool      flg, print_result = PETSC_TRUE;
  
  char           filein[PETSC_MAX_PATH_LEN],buf[PETSC_MAX_PATH_LEN];
  FILE*          file; 
  PetscInt       k,m,n,nnz,col,row;
  PetscScalar    val;


  PetscInitialize(&argc,&args,(char *)0,help);
  ierr = MPI_Comm_size(PETSC_COMM_WORLD,&size);CHKERRQ(ierr);
  if (size != 1) SETERRQ(PETSC_COMM_WORLD,1,"This is a uniprocessor program!");

  ierr = PetscOptionsGetString(PETSC_NULL,"-f",filein,PETSC_MAX_PATH_LEN,&flg);CHKERRQ(ierr);
  if (!flg) {
    SETERRQ(PETSC_COMM_WORLD,1,"Must indicate matrix file with the -f option");
  }

  /* Read Matrix Market File */

  ierr = PetscFOpen(PETSC_COMM_SELF,filein,"r",&file);CHKERRQ(ierr);

  /* process header with comments and if matrix is real and symmetric */

  PetscBool isSymmetric = PETSC_FALSE;
  PetscBool isReal = PETSC_FALSE;

  do {
    fgets(buf,PETSC_MAX_PATH_LEN-1,file);
    if(strstr(buf, "symmetric") != NULL){
      isSymmetric = PETSC_TRUE;
    }
    if(strstr(buf, "real") != NULL){
      isReal = PETSC_TRUE;
    }
  }while (buf[0] == '%');

  if(!isReal) SETERRQ(PETSC_COMM_WORLD,1,"This program requires a real matrix!");

  /* The first non-comment line has the matrix dimensions */
  sscanf(buf,"%d %d %d\n",&m,&n,&nnz);

  /* Create the vectors */
  ierr = VecCreate(PETSC_COMM_WORLD,&b);CHKERRQ(ierr);
  ierr = VecSetSizes(b,PETSC_DECIDE,n);CHKERRQ(ierr);
  ierr = VecSetFromOptions(b);CHKERRQ(ierr);
  
  ierr = VecSet(b,1);CHKERRQ(ierr);

  ierr = VecDuplicate(b,&x);CHKERRQ(ierr);
  ierr = VecDuplicate(b,&u);CHKERRQ(ierr);
  ierr = VecDuplicate(b,&d);CHKERRQ(ierr);

  ierr = MatCreateSeqAIJ(PETSC_COMM_WORLD,m,n,nnz*2/m,0,&A);CHKERRQ(ierr);
  ierr = MatSetOption(A,MAT_NEW_NONZERO_ALLOCATION_ERR,PETSC_FALSE);CHKERRQ(ierr); 

  /* Read matrix market file and insert values to the matrix A */
  for (k=0; k<nnz; k++) {
    fscanf(file,"%d %d %le\n",&row,&col,(double*)&val);
    row = row-1; col = col-1 ;
    ierr = MatSetValues(A,1,&row,1,&col,&val,INSERT_VALUES);CHKERRQ(ierr);
    if(isSymmetric){ // If matrix is symmetric, insert the same value on the other side of the diagonal
      if (row != col){
        ierr = MatSetValues(A,1,&col,1,&row,&val,INSERT_VALUES);CHKERRQ(ierr);
      }
    }
    // If matrix has a diagonal entry, set 1 at that location of vector d, we'll use this to fix missing diagonal entries
    if(row == col)
       VecSetValue(d,row,1,ADD_VALUES);
  }

  VecAssemblyBegin(d); VecAssemblyEnd(d);
  PetscScalar dv[1];
  val = 0;
  for(k=0; k<m; k++){
    VecGetValues(d,1,&k,dv);
    // If the value of an entry is 0, the matrix is missing the diagonal entry at that location
    if(dv[0] == 0)
      // Insert a zero
      ierr = MatSetValues(A,1,&k,1,&k,&val,INSERT_VALUES);CHKERRQ(ierr);
  }

  fclose(file);

  ierr = MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);

  //MatGetDiagonal(A,u);
  //VecView(d,PETSC_VIEWER_STDOUT_WORLD);
  MatView(A,PETSC_VIEWER_STDOUT_WORLD);

//   /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//                     1. KSPCG + PCILU(0)
//   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = KSPCreate(PETSC_COMM_WORLD,&ksp);CHKERRQ(ierr); 
  ierr = KSPSetOperators(ksp,A,A,SAME_NONZERO_PATTERN);CHKERRQ(ierr);
  ierr = KSPSetInitialGuessNonzero(ksp,PETSC_FALSE);CHKERRQ(ierr); 

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPCG);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCILU);CHKERRQ(ierr);
  ierr = PCFactorSetLevels(pc,0);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], " KSPCG     + PCILU(0)");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    2. KSPCG + PCILU(1)
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPCG);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCILU);CHKERRQ(ierr);
  ierr = PCFactorSetLevels(pc,1);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], " KSPCG     + PCILU(1)");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    3. KSPCG + PCILU(2)
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPCG);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCILU);CHKERRQ(ierr);
  ierr = PCFactorSetLevels(pc,2);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], " KSPCG     + PCILU(2)");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    4. KSPCG + PCJACOBI 
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPCG);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCJACOBI);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], " KSPCG     + PCJACOBI");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    5. KSPCG + PCSOR
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPCG);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCSOR);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], " KSPCG     + PCSOR   ");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

//   /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//                     6. KSPCG + PCNONE
//   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPCG);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCNONE);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], " KSPCG     + PCNONE  ");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

//   /* ================================================================ */
//   /* ================================================================ */

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    7. KSPCGS + PCILU(0)
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPCGS);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCILU);CHKERRQ(ierr);
  ierr = PCFactorSetLevels(pc,0);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], " KSPCGS    + PCILU(0)");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    8. KSPCGS + PCILU(1)
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPCGS);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCILU);CHKERRQ(ierr);
  ierr = PCFactorSetLevels(pc,1);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], " KSPCGS    + PCILU(1)");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    9. KSPCGS + PCILU(2)
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPCGS);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCILU);CHKERRQ(ierr);
  ierr = PCFactorSetLevels(pc,2);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], " KSPCGS    + PCILU(2)");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    10. KSPCGS + PCJACOBI 
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPCGS);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCJACOBI);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPCGS    + PCJACOBI");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    11. KSPCGS + PCSOR
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPCGS);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCSOR);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPCGS    + PCSOR   ");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    12. KSPCGS + PCNONE
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPCGS);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCNONE);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPCGS    + PCNONE  ");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

//   /* ================================================================ */
//   /* ================================================================ */

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    13. KSPBICG + PCILU(0)
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPBICG);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCILU);CHKERRQ(ierr);
  ierr = PCFactorSetLevels(pc,0);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPBICG   + PCILU(0)");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    14. KSPBICG + PCILU(1)
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPBICG);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCILU);CHKERRQ(ierr);
  ierr = PCFactorSetLevels(pc,1);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPBICG   + PCILU(1)");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    15. KSPBICG + PCILU(2)
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPBICG);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCILU);CHKERRQ(ierr);
  ierr = PCFactorSetLevels(pc,2);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPBICG   + PCILU(2)");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    16. KSPBICG + PCJACOBI 
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPBICG);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCJACOBI);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPBICG   + PCJACOBI");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    17. KSPBICG + PCNONE
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPBICG);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCNONE);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPBICG   + PCNONE  ");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

//   /* ================================================================ */
//   /* ================================================================ */

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    18. KSPBCGS + PCILU(0)
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPBCGS);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCILU);CHKERRQ(ierr);
  ierr = PCFactorSetLevels(pc,0);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPBCGS   + PCILU(0)");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    19. KSPBCGS + PCILU(1)
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPBCGS);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCILU);CHKERRQ(ierr);
  ierr = PCFactorSetLevels(pc,1);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPBCGS   + PCILU(1)");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    20. KSPBCGS + PCILU(2)
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPBCGS);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCILU);CHKERRQ(ierr);
  ierr = PCFactorSetLevels(pc,2);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPBCGS   + PCILU(2)");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    21. KSPBCGS + PCJACOBI 
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPBCGS);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCJACOBI);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPBCGS   + PCJACOBI");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    22. KSPBCGS + PCSOR
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPBCGS);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCSOR);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPBCGS   + PCSOR   ");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    23. KSPBCGS + PCNONE
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPBCGS);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCNONE);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPBCGS   + PCNONE  ");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

//   /* ================================================================ */
//   /* ================================================================ */

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    24. KSPGMRES + PCILU(0)
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPGMRES);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCILU);CHKERRQ(ierr);
  ierr = PCFactorSetLevels(pc,0);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPGMRES  + PCILU(0)");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    25. KSPGMRES + PCILU(1)
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPGMRES);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCILU);CHKERRQ(ierr);
  ierr = PCFactorSetLevels(pc,1);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPGMRES  + PCILU(1)");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    26. KSPGMRES + PCILU(2)
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPGMRES);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCILU);CHKERRQ(ierr);
  ierr = PCFactorSetLevels(pc,2);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPGMRES  + PCILU(2)");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    27. KSPGMRES + PCJACOBI 
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPGMRES);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCJACOBI);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPGMRES  + PCJACOBI");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    28. KSPGMRES + PCSOR
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPGMRES);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCSOR);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPGMRES  + PCSOR   ");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    29. KSPGMRES + PCNONE
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPGMRES);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCNONE);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPGMRES  + PCNONE  ");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

  /* ================================================================ */
  /* ================================================================ */

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    30. KSPTFQMR + PCILU(0)
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPTFQMR);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCILU);CHKERRQ(ierr);
  ierr = PCFactorSetLevels(pc,0);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPTFQMR  + PCILU(0)");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    31. KSPTFQMR + PCILU(1)
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPTFQMR);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCILU);CHKERRQ(ierr);
  ierr = PCFactorSetLevels(pc,1);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPTFQMR  + PCILU(1)");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    32. KSPTFQMR + PCILU(2)
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPTFQMR);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCILU);CHKERRQ(ierr);
  ierr = PCFactorSetLevels(pc,2);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPTFQMR  + PCILU(2)");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    33. KSPTFQMR + PCJACOBI 
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPTFQMR);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCJACOBI);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPTFQMR  + PCJACOBI");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    34. KSPTFQMR + PCSOR
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPTFQMR);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCSOR);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPTFQMR  + PCSOR   ");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    35. KSPTFQMR + PCNONE
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPTFQMR);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCNONE);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPTFQMR  + PCNONE  ");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

  /* ================================================================ */
  /* ================================================================ */

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    36. KSPFGMRES + PCILU(0)
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
  ierr = KSPDestroy(&ksp);CHKERRQ(ierr);
  ierr = KSPCreate(PETSC_COMM_WORLD,&ksp);CHKERRQ(ierr); 
  ierr = KSPSetOperators(ksp,A,A,SAME_NONZERO_PATTERN);CHKERRQ(ierr);
  ierr = KSPSetInitialGuessNonzero(ksp,PETSC_FALSE);CHKERRQ(ierr); 

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPFGMRES);CHKERRQ(ierr);
  ierr = KSPSetPCSide(ksp,PC_RIGHT);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCILU);CHKERRQ(ierr);
  ierr = PCFactorSetLevels(pc,0);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPFGMRES + PCILU(0)");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    37. KSPFGMRES + PCILU(1)
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPFGMRES);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCILU);CHKERRQ(ierr);
  ierr = PCFactorSetLevels(pc,1);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPFGMRES + PCILU(1)");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    38. KSPFGMRES + PCILU(2)
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPFGMRES);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCILU);CHKERRQ(ierr);
  ierr = PCFactorSetLevels(pc,2);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPFGMRES + PCILU(2)");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    39. KSPFGMRES + PCJACOBI 
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPFGMRES);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCJACOBI);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPFGMRES + PCJACOBI");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    40. KSPFGMRES + PCSOR
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPFGMRES);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCSOR);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPFGMRES + PCSOR   ");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                    41. KSPFGMRES + PCNONE
  - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PetscTime(&tsetup1);CHKERRQ(ierr);
  ierr = KSPSetType(ksp,KSPFGMRES);CHKERRQ(ierr);
  ierr = KSPGetPC(ksp,&pc);CHKERRQ(ierr); 
  ierr = PCSetType(pc,PCNONE);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
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

  strcpy(settings[i], "KSPFGMRES + PCNONE  ");
  if(print_result){
    ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. %s",i,settings[i]);CHKERRQ(ierr);    
    ierr = PetscPrintf(PETSC_COMM_WORLD," | %e | %11G | %5D\n",tsetup[i]+tsolve[i],norm[i],its[i]);CHKERRQ(ierr);
  }
  i++;

  /* ================================================================ */
  /* ================================================================ */


  //VecView(x,PETSC_VIEWER_STDOUT_WORLD);

  //ierr = PetscViewerBinaryOpen(PETSC_COMM_WORLD,"solution.petsc",FILE_MODE_WRITE,&viewer);CHKERRQ(ierr);
  //ierr = VecView(x, viewer);CHKERRQ(ierr);
  //ierr = PetscViewerDestroy(&viewer);CHKERRQ(ierr);
  /* 
     Free work space.  All PETSc objects should be destroyed when they
     are no longer needed.
  */

  // for(j = 1; j < i; j++){
  //     ierr = PetscPrintf(PETSC_COMM_WORLD,"%D. ",j);CHKERRQ(ierr);
  //     ierr = PetscPrintf(PETSC_COMM_WORLD,settings[j]);CHKERRQ(ierr);
  //     ierr = PetscPrintf(PETSC_COMM_WORLD," | %11G + %5D\n",norm[i],its[i]);CHKERRQ(ierr);
  // }

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
