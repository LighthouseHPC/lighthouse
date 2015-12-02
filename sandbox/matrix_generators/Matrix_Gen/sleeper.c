/*
   This example implements one of the problems found at
       NLEVP: A Collection of Nonlinear Eigenvalue Problems,
       The University of Manchester.
   The details of the collection can be found at:
       [1] T. Betcke et al., "NLEVP: A Collection of Nonlinear Eigenvalue
           Problems", ACM Trans. Math. Software 39(2), Article 7, 2013.

   The sleeper problem is a proportionally damped QEP describing the
   oscillations of a rail track resting on sleepers.
*/

static char help[] = "NLEVP problem: sleeper.\n\n"
  "The command line options are:\n"
  "  -n <n>, where <n> = dimension of the matrices.\n\n";

#include <slepcpep.h>
#include <petsctime.h>

#undef __FUNCT__
#define __FUNCT__ "main"
int main(int argc,char **argv)
{
  Mat            M,C,K,A[3];      /* problem matrices */
  PetscInt       n=10,Istart,Iend,i;
  PetscErrorCode ierr;
  PetscViewer    view;
  char           file[PETSC_MAX_PATH_LEN];

  SlepcInitialize(&argc,&argv,(char*)0,help);

  ierr = PetscOptionsGetInt(NULL,"-n",&n,NULL);CHKERRQ(ierr);
  ierr = PetscPrintf(PETSC_COMM_WORLD,"\nRailtrack resting on sleepers, n=%D\n\n",n);CHKERRQ(ierr);

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
     Compute the matrices that define the eigensystem, (k^2*M+k*C+K)x=0
     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  /* K is a pentadiagonal */
  ierr = MatCreate(PETSC_COMM_WORLD,&K);CHKERRQ(ierr);
  ierr = MatSetSizes(K,PETSC_DECIDE,PETSC_DECIDE,n,n);CHKERRQ(ierr);
  ierr = MatSetFromOptions(K);CHKERRQ(ierr);
  ierr = MatSetUp(K);CHKERRQ(ierr);
  
  ierr = MatGetOwnershipRange(K,&Istart,&Iend);CHKERRQ(ierr);
  for (i=Istart;i<Iend;i++) {
    if (i==0) { 
      ierr = MatSetValue(K,i,n-1,-3.0,INSERT_VALUES);CHKERRQ(ierr);
      ierr = MatSetValue(K,i,n-2,1.0,INSERT_VALUES);CHKERRQ(ierr);
    }
    if (i==1) { ierr = MatSetValue(K,i,n-1,1.0,INSERT_VALUES);CHKERRQ(ierr); }
    if (i>0) { ierr = MatSetValue(K,i,i-1,-3.0,INSERT_VALUES);CHKERRQ(ierr); }
    if (i>1) { ierr = MatSetValue(K,i,i-2,1.0,INSERT_VALUES);CHKERRQ(ierr); }
    ierr = MatSetValue(K,i,i,5.0,INSERT_VALUES);CHKERRQ(ierr);
    if (i==n-1) { 
      ierr = MatSetValue(K,i,0,-3.0,INSERT_VALUES);CHKERRQ(ierr);
      ierr = MatSetValue(K,i,1,1.0,INSERT_VALUES);CHKERRQ(ierr);
    }
    if (i==n-2) { ierr = MatSetValue(K,i,0,1.0,INSERT_VALUES);CHKERRQ(ierr); }
    if (i<n-1) { ierr = MatSetValue(K,i,i+1,-3.0,INSERT_VALUES);CHKERRQ(ierr); }
    if (i<n-2) { ierr = MatSetValue(K,i,i+2,1.0,INSERT_VALUES);CHKERRQ(ierr); }
  }

  ierr = MatAssemblyBegin(K,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(K,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);

  /* C is a circulant matrix */
  ierr = MatCreate(PETSC_COMM_WORLD,&C);CHKERRQ(ierr);
  ierr = MatSetSizes(C,PETSC_DECIDE,PETSC_DECIDE,n,n);CHKERRQ(ierr);
  ierr = MatSetFromOptions(C);CHKERRQ(ierr);
  ierr = MatSetUp(C);CHKERRQ(ierr);
  
  ierr = MatGetOwnershipRange(C,&Istart,&Iend);CHKERRQ(ierr);
  for (i=Istart;i<Iend;i++) {
    if (i==0) { 
      ierr = MatSetValue(C,i,n-1,-4.0,INSERT_VALUES);CHKERRQ(ierr);
      ierr = MatSetValue(C,i,n-2,1.0,INSERT_VALUES);CHKERRQ(ierr);
    }
    if (i==1) { ierr = MatSetValue(C,i,n-1,1.0,INSERT_VALUES);CHKERRQ(ierr); }
    if (i>0) { ierr = MatSetValue(C,i,i-1,-4.0,INSERT_VALUES);CHKERRQ(ierr); }
    if (i>1) { ierr = MatSetValue(C,i,i-2,1.0,INSERT_VALUES);CHKERRQ(ierr); }
    ierr = MatSetValue(C,i,i,7.0,INSERT_VALUES);CHKERRQ(ierr);
    if (i==n-1) { 
      ierr = MatSetValue(C,i,0,-4.0,INSERT_VALUES);CHKERRQ(ierr);
      ierr = MatSetValue(C,i,1,1.0,INSERT_VALUES);CHKERRQ(ierr);
    }
    if (i==n-2) { ierr = MatSetValue(C,i,0,1.0,INSERT_VALUES);CHKERRQ(ierr); }
    if (i<n-1) { ierr = MatSetValue(C,i,i+1,-4.0,INSERT_VALUES);CHKERRQ(ierr); }
    if (i<n-2) { ierr = MatSetValue(C,i,i+2,1.0,INSERT_VALUES);CHKERRQ(ierr); }
  }

  ierr = MatAssemblyBegin(C,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(C,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  
  /* M is the identity matrix */
  ierr = MatCreate(PETSC_COMM_WORLD,&M);CHKERRQ(ierr);
  ierr = MatSetSizes(M,PETSC_DECIDE,PETSC_DECIDE,n,n);CHKERRQ(ierr);
  ierr = MatSetFromOptions(M);CHKERRQ(ierr);
  ierr = MatSetUp(M);CHKERRQ(ierr);
  ierr = MatGetOwnershipRange(M,&Istart,&Iend);CHKERRQ(ierr);
  for (i=Istart;i<Iend;i++) {
    ierr = MatSetValue(M,i,i,1.0,INSERT_VALUES);CHKERRQ(ierr);
  }
  ierr = MatAssemblyBegin(M,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(M,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);

  sprintf(file, "sleeper_m_n%d.petsc", n);
  ierr = PetscViewerBinaryOpen(PETSC_COMM_WORLD,file,FILE_MODE_WRITE,&view);CHKERRQ(ierr);
  ierr = MatView(M,view);CHKERRQ(ierr);
  ierr = MatDestroy(&M);CHKERRQ(ierr);

  sprintf(file, "sleeper_n%d.petsc", n);
  ierr = PetscViewerBinaryOpen(PETSC_COMM_WORLD,file,FILE_MODE_WRITE,&view);CHKERRQ(ierr);
  ierr = MatView(C,view);CHKERRQ(ierr);
  ierr = MatDestroy(&C);CHKERRQ(ierr);

  sprintf(file, "sleeper_k_n%d.petsc", n);
  ierr = PetscViewerBinaryOpen(PETSC_COMM_WORLD,file,FILE_MODE_WRITE,&view);CHKERRQ(ierr);
  ierr = MatView(K,view);CHKERRQ(ierr);
  ierr = MatDestroy(&K);CHKERRQ(ierr);

  ierr = MatDestroy(&M);CHKERRQ(ierr);
  ierr = MatDestroy(&C);CHKERRQ(ierr);
  ierr = MatDestroy(&K);CHKERRQ(ierr);
  ierr = SlepcFinalize();CHKERRQ(ierr);
  return 0;
}
