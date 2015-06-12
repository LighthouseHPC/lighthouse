static char help[] = "Lighthouse's Quadratic Eigenvalue Solver Tester.\n\n"
  "The command line options are:\n"
  "  -M <M>, where <M> = The mass matric.\n"
  "  -C <C>, where <C> = The damping matrix.\n"
  "  -K <K>, where <K> = The stiffness matrix.\n\n";

#include <slepcpep.h>
#include <petsctime.h>
int main(int argc,char **argv)
{
  Mat            M,K,C,A[3];      /* problem matrices */
  PEP            pep;             /* polynomial eigenproblem solver context */
  PEPType        type;
  char		 filename[PETSC_MAX_PATH_LEN];
  PetscLogDouble time1,time2;
  PetscBool      flag;
  PetscViewer	 viewer;
  PetscErrorCode ierr;

  SlepcInitialize(&argc,&argv,(char*)0,help);

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
     Load the matrices that define the eigensystem, (k^2*M+k*C+K)x=0
     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
  /*
     Load the mass matrix, M.
  */
  ierr = PetscOptionsGetString(NULL,"-M",filename,PETSC_MAX_PATH_LEN,&flag);CHKERRQ(ierr);
  if(!flag) SETERRQ(PETSC_COMM_WORLD,1,"Must indicate a file name for matrix M with the -M option");
  ierr = PetscViewerBinaryOpen(PETSC_COMM_WORLD,filename,FILE_MODE_READ,&viewer);CHKERRQ(ierr);
  ierr = MatCreate(PETSC_COMM_WORLD,&M); CHKERRQ(ierr);
  ierr = MatSetFromOptions(M);CHKERRQ(ierr);
  ierr = MatLoad(M,viewer); CHKERRQ(ierr);
  ierr = PetscViewerDestroy(&viewer);

  /*
     Load the damping matrix, K.
  */
  ierr = PetscOptionsGetString(NULL,"-K",filename,PETSC_MAX_PATH_LEN,&flag);CHKERRQ(ierr);
  if(!flag) SETERRQ(PETSC_COMM_WORLD,1,"Must indicate a file name for matrix K with the -K option");
  ierr = PetscViewerBinaryOpen(PETSC_COMM_WORLD,filename,FILE_MODE_READ,&viewer);CHKERRQ(ierr);
  ierr = MatCreate(PETSC_COMM_WORLD,&K); CHKERRQ(ierr);
  ierr = MatSetFromOptions(K);CHKERRQ(ierr);
  ierr = MatLoad(K,viewer); CHKERRQ(ierr);
  ierr = PetscViewerDestroy(&viewer);

  /*
     Load the stiffness matrix, C.
  */
  ierr = PetscOptionsGetString(NULL,"-C",filename,PETSC_MAX_PATH_LEN,&flag);CHKERRQ(ierr);
  if(!flag) SETERRQ(PETSC_COMM_WORLD,1,"Must indicate a file name for matrix C with the -C option");
  ierr = PetscViewerBinaryOpen(PETSC_COMM_WORLD,filename,FILE_MODE_READ,&viewer);CHKERRQ(ierr);
  ierr = MatCreate(PETSC_COMM_WORLD,&C); CHKERRQ(ierr);
  ierr = MatSetFromOptions(C);CHKERRQ(ierr);
  ierr = MatLoad(C,viewer); CHKERRQ(ierr);
  ierr = PetscViewerDestroy(&viewer);
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                Create the eigensolver and set various options
     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  /*
     Create eigensolver context
  */
  ierr = PEPCreate(PETSC_COMM_WORLD,&pep);CHKERRQ(ierr);

  /*
     Set matrices and problem type
  */
  A[0] = K; A[1] = C; A[2] = M;
  ierr = PEPSetOperators(pep,3,A);CHKERRQ(ierr);

  /*
     Set solver parameters at runtime
  */
  ierr = PEPSetFromOptions(pep);CHKERRQ(ierr);

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                      Solve the eigensystem
     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
  PetscTime(&time1);
  ierr = PEPSolve(pep);CHKERRQ(ierr);
  PetscTime(&time2);

  /*
     Optional: Get some information from the solver and display it
  */
  ierr = PEPGetType(pep,&type);CHKERRQ(ierr);
  ierr = PetscPrintf(PETSC_COMM_WORLD," Solution method: %s\n\n",type);CHKERRQ(ierr);

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                    Display solution and clean up
     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PEPPrintSolution(pep,NULL);CHKERRQ(ierr);
  ierr = PetscPrintf(PETSC_COMM_WORLD,"Time: %g\n",time2-time1);
  ierr = PEPDestroy(&pep);CHKERRQ(ierr);
  ierr = MatDestroy(&M);CHKERRQ(ierr);
  ierr = MatDestroy(&C);CHKERRQ(ierr);
  ierr = MatDestroy(&K);CHKERRQ(ierr);
  ierr = SlepcFinalize();
  return 0;
}

