/*
   This example implements one of the problems found at
       NLEVP: A Collection of Nonlinear Eigenvalue Problems,
       The University of Manchester.
   The details of the collection can be found at:
       [1] T. Betcke et al., "NLEVP: A Collection of Nonlinear Eigenvalue
           Problems", ACM Trans. Math. Software 39(2), Article 7, 2013.

   The acoustic_wave_1d problem is a QEP from an acoustics application.
   Here we solve it with the eigenvalue scaled by the imaginary unit, to be
   able to use real arithmetic, so the computed eigenvalues should be scaled
   back.
*/

static char help[] = "NLEVP problem: acoustic_wave_1d.\n\n"
  "The command line options are:\n"
  "  -n <n>, where <n> = dimension of the matrices.\n"
  "  -z <z>, where <z> = impedance (default 1.0).\n\n";

#include <slepcpep.h>
#undef __FUNCT__
#define __FUNCT__ "main"
int main(int argc,char **argv)
{
  Mat            M,C,K;      /* problem matrices */            /* polynomial eigenproblem solver context */
  PetscInt       n=10,Istart,Iend,i;
  PetscScalar    z=1.0;
  char           str[50],file[PETSC_MAX_PATH_LEN];
  PetscErrorCode ierr;
  PetscViewer    view;

  SlepcInitialize(&argc,&argv,(char*)0,help);

  ierr = PetscOptionsGetInt(NULL,"-n",&n,NULL);CHKERRQ(ierr);
  ierr = PetscOptionsGetScalar(NULL,"-z",&z,NULL);CHKERRQ(ierr);
  ierr = SlepcSNPrintfScalar(str,50,z,PETSC_FALSE);CHKERRQ(ierr);
  ierr = PetscPrintf(PETSC_COMM_WORLD,"\nAcoustic wave 1-D, n=%D z=%s\n\n",n,str);CHKERRQ(ierr);

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
     Compute the matrices that define the eigensystem, (k^2*M+k*C+K)x=0
     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  /* K is a tridiagonal */
  ierr = MatCreate(PETSC_COMM_WORLD,&K);CHKERRQ(ierr);
  ierr = MatSetSizes(K,PETSC_DECIDE,PETSC_DECIDE,n,n);CHKERRQ(ierr);
  ierr = MatSetFromOptions(K);CHKERRQ(ierr);
  ierr = MatSetUp(K);CHKERRQ(ierr);
  
  ierr = MatGetOwnershipRange(K,&Istart,&Iend);CHKERRQ(ierr);
  for (i=Istart;i<Iend;i++) {
    if (i>0) {
      ierr = MatSetValue(K,i,i-1,-1.0*n,INSERT_VALUES);CHKERRQ(ierr);
    }
    if (i<n-1) {
      ierr = MatSetValue(K,i,i,2.0*n,INSERT_VALUES);CHKERRQ(ierr);
      ierr = MatSetValue(K,i,i+1,-1.0*n,INSERT_VALUES);CHKERRQ(ierr);
    } else {
      ierr = MatSetValue(K,i,i,1.0*n,INSERT_VALUES);CHKERRQ(ierr);
    }
  }

  ierr = MatAssemblyBegin(K,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(K,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);

  /* C is the zero matrix but one element*/
  ierr = MatCreate(PETSC_COMM_WORLD,&C);CHKERRQ(ierr);
  ierr = MatSetSizes(C,PETSC_DECIDE,PETSC_DECIDE,n,n);CHKERRQ(ierr);
  ierr = MatSetFromOptions(C);CHKERRQ(ierr);
  ierr = MatSetUp(C);CHKERRQ(ierr);

  ierr = MatGetOwnershipRange(C,&Istart,&Iend);CHKERRQ(ierr);
  if (n-1>=Istart && n-1<Iend) { 
    ierr = MatSetValue(C,n-1,n-1,-2*PETSC_PI/z,INSERT_VALUES);CHKERRQ(ierr);
  }
  ierr = MatAssemblyBegin(C,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(C,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  
  /* M is a diagonal matrix */
  ierr = MatCreate(PETSC_COMM_WORLD,&M);CHKERRQ(ierr);
  ierr = MatSetSizes(M,PETSC_DECIDE,PETSC_DECIDE,n,n);CHKERRQ(ierr);
  ierr = MatSetFromOptions(M);CHKERRQ(ierr);
  ierr = MatSetUp(M);CHKERRQ(ierr);

  ierr = MatGetOwnershipRange(M,&Istart,&Iend);CHKERRQ(ierr);
  for (i=Istart;i<Iend;i++) {
    if (i<n-1) {
      ierr = MatSetValue(M,i,i,4*PETSC_PI*PETSC_PI/n,INSERT_VALUES);CHKERRQ(ierr);
    } else {
      ierr = MatSetValue(M,i,i,2*PETSC_PI*PETSC_PI/n,INSERT_VALUES);CHKERRQ(ierr);
    }
  }
  ierr = MatAssemblyBegin(M,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(M,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  
  sprintf(file, "acoustic_wave_1d_m_n%d_z%.1f.petsc", n,z);
  ierr = PetscViewerBinaryOpen(PETSC_COMM_WORLD,file,FILE_MODE_WRITE,&view);CHKERRQ(ierr);
  ierr = MatView(M,view);CHKERRQ(ierr);
  ierr = MatDestroy(&M);CHKERRQ(ierr);

  sprintf(file, "acoustic_wave_1d_c_n%d_z%.1f.petsc", n,z);
  ierr = PetscViewerBinaryOpen(PETSC_COMM_WORLD,file,FILE_MODE_WRITE,&view);CHKERRQ(ierr);
  ierr = MatView(C,view);CHKERRQ(ierr);
  ierr = MatDestroy(&C);CHKERRQ(ierr);

  sprintf(file, "acoustic_wave_1d_k_n%d_z%.1f.petsc", n,z);
  ierr = PetscViewerBinaryOpen(PETSC_COMM_WORLD,file,FILE_MODE_WRITE,&view);CHKERRQ(ierr);
  ierr = MatView(K,view);CHKERRQ(ierr);
  ierr = MatDestroy(&K);CHKERRQ(ierr);

  ierr = SlepcFinalize();CHKERRQ(ierr);
  return 0;
}
