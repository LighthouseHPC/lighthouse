static char help[] = "Reads a matrix and rhs from a petsc binary file and solves the linear system.\n\
  -f <input_file> : petsc binary file containing matrix and RHS\n\
  -trans  : solve transpose system instead\n\
  -output_solution : write solution in petsc binary format\n\n";

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
  PetscInt       its;
  PetscMPIInt    size;
  PetscReal      norm;
  PetscLogDouble solveTime,endTime,startTime;
  PetscBool      flg,trans=PETSC_FALSE;
  PetscBool      isSymmetric,outputSoln=PETSC_FALSE;
  
  PetscViewer    fd;              /* viewer */
  char           file[2][PETSC_MAX_PATH_LEN];

  PetscInitialize(&argc,&args,(char *)0,help);
  ierr = MPI_Comm_size(PETSC_COMM_WORLD,&size);CHKERRQ(ierr);
  // check command line options
  ierr = PetscOptionsGetString(NULL,"-f",file[0],PETSC_MAX_PATH_LEN,&flg);CHKERRQ(ierr);
  ierr = PetscOptionsGetBool(NULL,"-trans",&trans,NULL);CHKERRQ(ierr);
  ierr = PetscOptionsGetBool(NULL,"-output_solution",&outputSoln,NULL);CHKERRQ(ierr);

  if (!flg) {
    SETERRQ(PETSC_COMM_WORLD,1,"Must indicate matrix file with the -f option");
  }
  // open petsc binary file reader
  ierr = PetscViewerBinaryOpen(PETSC_COMM_WORLD,file[0],FILE_MODE_READ,&fd);CHKERRQ(ierr);
  // create matrix and vectors and load data from file
  ierr = MatCreate(PETSC_COMM_WORLD,&A);CHKERRQ(ierr);
  ierr = MatSetFromOptions(A);CHKERRQ(ierr);
  ierr = MatLoad(A,fd);CHKERRQ(ierr);
  ierr = VecCreate(PETSC_COMM_WORLD,&b);CHKERRQ(ierr);
  ierr = VecSetFromOptions(b);CHKERRQ(ierr);
  ierr = VecLoad(b,fd);CHKERRQ(ierr);
  ierr = PetscViewerDestroy(&fd);CHKERRQ(ierr);
  // assemble matrix
  ierr = MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  // create vector for exact solution and norm calculation
  ierr = VecDuplicate(b,&x);CHKERRQ(ierr);
  ierr = VecDuplicate(b,&u);CHKERRQ(ierr);

  /* Check whether A is symmetric */
  flg  = PETSC_FALSE;
  ierr = PetscOptionsGetBool(NULL, "-check_symmetry", &flg,NULL);CHKERRQ(ierr);
  if (flg) {
    Mat Atrans;
    ierr = MatTranspose(A, MAT_INITIAL_MATRIX,&Atrans);
    ierr = MatEqual(A, Atrans, &isSymmetric);
    if (isSymmetric) {
      ierr = MatSetOption(A,MAT_SYMMETRIC,PETSC_TRUE);CHKERRQ(ierr);
    } else {
      PetscPrintf(PETSC_COMM_WORLD,"Warning: A is non-symmetric \n");CHKERRQ(ierr);
    }
    ierr = MatDestroy(&Atrans);CHKERRQ(ierr);
  }

  /* Check scaling in A */
  flg  = PETSC_FALSE;
  ierr = PetscOptionsGetBool(NULL, "-check_scaling", &flg,NULL);CHKERRQ(ierr);
  if (flg) {
    Vec       max, min;
    PetscInt  idx;
    PetscReal val;

    ierr = VecDuplicate(x, &max);CHKERRQ(ierr);
    ierr = VecDuplicate(x, &min);CHKERRQ(ierr);
    ierr = MatGetRowMaxAbs(A, max, NULL);CHKERRQ(ierr);
    ierr = MatGetRowMinAbs(A, min, NULL);CHKERRQ(ierr);
    {
      PetscViewer viewer;

      ierr = PetscViewerASCIIOpen(PETSC_COMM_WORLD, "max.data", &viewer);CHKERRQ(ierr);
      ierr = VecView(max, viewer);CHKERRQ(ierr);
      ierr = PetscViewerDestroy(&viewer);CHKERRQ(ierr);
      ierr = PetscViewerASCIIOpen(PETSC_COMM_WORLD, "min.data", &viewer);CHKERRQ(ierr);
      ierr = VecView(min, viewer);CHKERRQ(ierr);
      ierr = PetscViewerDestroy(&viewer);CHKERRQ(ierr);
    }
    ierr = VecView(max, PETSC_VIEWER_DRAW_WORLD);CHKERRQ(ierr);
    ierr = VecMax(max, &idx, &val);CHKERRQ(ierr);
    ierr = PetscPrintf(PETSC_COMM_WORLD, "Largest max row element %G at row %d\n", val, idx);CHKERRQ(ierr);
    ierr = VecView(min, PETSC_VIEWER_DRAW_WORLD);CHKERRQ(ierr);
    ierr = VecMin(min, &idx, &val);CHKERRQ(ierr);
    ierr = PetscPrintf(PETSC_COMM_WORLD, "Smallest min row element %G at row %d\n", val, idx);CHKERRQ(ierr);
    ierr = VecMin(max, &idx, &val);CHKERRQ(ierr);
    ierr = PetscPrintf(PETSC_COMM_WORLD, "Smallest max row element %G at row %d\n", val, idx);CHKERRQ(ierr);
    ierr = VecPointwiseDivide(max, max, min);CHKERRQ(ierr);
    ierr = VecMax(max, &idx, &val);CHKERRQ(ierr);
    ierr = PetscPrintf(PETSC_COMM_WORLD, "Largest row ratio %G at row %d\n", val, idx);CHKERRQ(ierr);
    ierr = VecView(max, PETSC_VIEWER_DRAW_WORLD);CHKERRQ(ierr);
    ierr = VecDestroy(&max);CHKERRQ(ierr);
    ierr = VecDestroy(&min);CHKERRQ(ierr);
  }

  // create KSP solver context
  ierr = KSPCreate(PETSC_COMM_WORLD,&ksp);CHKERRQ(ierr); 
  ierr = KSPSetOperators(ksp,A,A,SAME_NONZERO_PATTERN);CHKERRQ(ierr);
  ierr = KSPSetInitialGuessNonzero(ksp,PETSC_FALSE);CHKERRQ(ierr);
  // setup KSP
  ierr = KSPSetFromOptions(ksp);CHKERRQ(ierr);
  ierr = KSPSetUp(ksp);CHKERRQ(ierr);
  ierr = KSPSetUpOnBlocks(ksp);CHKERRQ(ierr);
  // remember start time
  ierr = PetscTime(&startTime);CHKERRQ(ierr);
  if (trans) {
    // solve transposed system
    ierr = KSPSolveTranspose(ksp,b,x);CHKERRQ(ierr);
  }else{
    // solve regular system
    ierr = KSPSolve(ksp,b,x);CHKERRQ(ierr); 
  }
  // get iteration number  
  ierr = KSPGetIterationNumber(ksp,&its);CHKERRQ(ierr);
  // compute residual norm
  if (trans) {
    ierr = MatMultTranspose(A,x,u);CHKERRQ(ierr);
  } else {
    ierr = MatMult(A,x,u);CHKERRQ(ierr);
  }
  ierr = VecAXPY(u,-1.0,b);CHKERRQ(ierr);
  ierr = VecNorm(u,NORM_2,&norm);CHKERRQ(ierr);
  // Compute solve time
  ierr = PetscTime(&endTime);CHKERRQ(ierr);
  solveTime = endTime - startTime;
  // Print iteration number, solve time, residual norm
  ierr = PetscPrintf(PETSC_COMM_WORLD,"  Number of iterations = %3D\n",its);CHKERRQ(ierr);
  ierr = PetscPrintf(PETSC_COMM_WORLD," Solve time = %es\n",solveTime);CHKERRQ(ierr);
  if (norm < 1.e-12) {
    ierr = PetscPrintf(PETSC_COMM_WORLD,"  Residual norm < 1.e-12\n");CHKERRQ(ierr);
  } else {
    ierr = PetscPrintf(PETSC_COMM_WORLD,"  Residual norm %G\n",norm);CHKERRQ(ierr);
  }  

  // If user wants to output solution
  if (outputSoln) {
    PetscViewer viewer;

    ierr = PetscViewerBinaryOpen(PETSC_COMM_WORLD,"solution.petsc",FILE_MODE_WRITE,&viewer);CHKERRQ(ierr);
    ierr = VecView(x, viewer);CHKERRQ(ierr);
    ierr = PetscViewerDestroy(&viewer);CHKERRQ(ierr);
  }

  // Destroy matrices and vector
  ierr = VecDestroy(&x);CHKERRQ(ierr);
  ierr = VecDestroy(&b);CHKERRQ(ierr); 
  ierr = MatDestroy(&A);CHKERRQ(ierr);
  ierr = KSPDestroy(&ksp);CHKERRQ(ierr);
  // We're done
  ierr = PetscFinalize();
  return 0;
}
