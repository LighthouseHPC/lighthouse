/*
   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
   SLEPc - Scalable Library for Eigenvalue Problem Computations
   Copyright (c) 2002-2015, Universitat Politecnica de Valencia, Spain

   This file is part of SLEPc.

   SLEPc is free software: you can redistribute it and/or modify it under  the
   terms of version 3 of the GNU Lesser General Public License as published by
   the Free Software Foundation.

   SLEPc  is  distributed in the hope that it will be useful, but WITHOUT  ANY
   WARRANTY;  without even the implied warranty of MERCHANTABILITY or  FITNESS
   FOR  A  PARTICULAR PURPOSE. See the GNU Lesser General Public  License  for
   more details.

   You  should have received a copy of the GNU Lesser General  Public  License
   along with SLEPc. If not, see <http://www.gnu.org/licenses/>.
   - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
*/
/*
   This example implements one of the problems found at
       NLEVP: A Collection of Nonlinear Eigenvalue Problems,
       The University of Manchester.
   The details of the collection can be found at:
       [1] T. Betcke et al., "NLEVP: A Collection of Nonlinear Eigenvalue
           Problems", ACM Trans. Math. Software 39(2), Article 7, 2013.

   The damped_beam problem is a QEP from the vibrarion analysis of a beam
   simply supported at both ends and damped in the middle.
*/

static char help[] = "NLEVP problem: damped_beam.\n\n"
  "The command line options are:\n"
  "  -n <n> ... dimension of the matrices.\n\n";

#include <slepcpep.h>
#include <petsctime.h>

#undef __FUNCT__
#define __FUNCT__ "main"
int main(int argc,char **argv)
{
  Mat            M,Mo,C,K,Ko,A[3]; /* problem matrices */
  PEP            pep;              /* polynomial eigenproblem solver context */
  IS             isf,isbc,is;
  PetscInt       n=200,nele,Istart,Iend,i,j,mloc,nloc,bc[2];
  PetscReal      width=0.05,height=0.005,glength=1.0,dlen,EI,area,rho;
  PetscScalar    K1[4],K2[4],K2t[4],K3[4],M1[4],M2[4],M2t[4],M3[4],damp=5.0;
  PetscBool      terse;
  PetscErrorCode ierr;
  PetscLogDouble time1,time2;

  SlepcInitialize(&argc,&argv,(char*)0,help);

  ierr = PetscOptionsGetInt(NULL,"-n",&n,NULL);CHKERRQ(ierr);
  nele = n/2;
  n    = 2*nele;
  ierr = PetscPrintf(PETSC_COMM_WORLD,"\nSimply supported beam damped in the middle, n=%D (nele=%D)\n\n",n,nele);CHKERRQ(ierr);

  dlen = glength/nele;
  EI   = 7e10*width*height*height*height/12.0;
  area = width*height;
  rho  = 0.674/(area*glength);

  K1[0]  =  12;  K1[1]  =   6*dlen;  K1[2]  =   6*dlen;  K1[3]  =  4*dlen*dlen;
  K2[0]  = -12;  K2[1]  =   6*dlen;  K2[2]  =  -6*dlen;  K2[3]  =  2*dlen*dlen;
  K2t[0] = -12;  K2t[1] =  -6*dlen;  K2t[2] =   6*dlen;  K2t[3] =  2*dlen*dlen;
  K3[0]  =  12;  K3[1]  =  -6*dlen;  K3[2]  =  -6*dlen;  K3[3]  =  4*dlen*dlen;
  M1[0]  = 156;  M1[1]  =  22*dlen;  M1[2]  =  22*dlen;  M1[3]  =  4*dlen*dlen;
  M2[0]  =  54;  M2[1]  = -13*dlen;  M2[2]  =  13*dlen;  M2[3]  = -3*dlen*dlen;
  M2t[0] =  54;  M2t[1] =  13*dlen;  M2t[2] = -13*dlen;  M2t[3] = -3*dlen*dlen;
  M3[0]  = 156;  M3[1]  = -22*dlen;  M3[2]  = -22*dlen;  M3[3]  =  4*dlen*dlen;

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
     Compute the matrices that define the eigensystem, (k^2*M+k*C+K)x=0
     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  /* K is block-tridiagonal */
  ierr = MatCreate(PETSC_COMM_WORLD,&Ko);CHKERRQ(ierr);
  ierr = MatSetSizes(Ko,PETSC_DECIDE,PETSC_DECIDE,n+2,n+2);CHKERRQ(ierr);
  ierr = MatSetBlockSize(Ko,2);CHKERRQ(ierr);
  ierr = MatSetFromOptions(Ko);CHKERRQ(ierr);
  ierr = MatSetUp(Ko);CHKERRQ(ierr);
  
  ierr = MatGetOwnershipRange(Ko,&Istart,&Iend);CHKERRQ(ierr);
  for (i=Istart/2;i<Iend/2;i++) {
    if (i>0) {
      j = i-1;
      ierr = MatSetValuesBlocked(Ko,1,&i,1,&j,K2t,ADD_VALUES);CHKERRQ(ierr);
      ierr = MatSetValuesBlocked(Ko,1,&i,1,&i,K3,ADD_VALUES);CHKERRQ(ierr);
    }
    if (i<nele) {
      j = i+1;
      ierr = MatSetValuesBlocked(Ko,1,&i,1,&j,K2,ADD_VALUES);CHKERRQ(ierr);
      ierr = MatSetValuesBlocked(Ko,1,&i,1,&i,K1,ADD_VALUES);CHKERRQ(ierr);
    }
  }
  ierr = MatAssemblyBegin(Ko,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(Ko,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatScale(Ko,EI/(dlen*dlen*dlen));CHKERRQ(ierr);

  /* M is block-tridiagonal */
  ierr = MatCreate(PETSC_COMM_WORLD,&Mo);CHKERRQ(ierr);
  ierr = MatSetSizes(Mo,PETSC_DECIDE,PETSC_DECIDE,n+2,n+2);CHKERRQ(ierr);
  ierr = MatSetBlockSize(Mo,2);CHKERRQ(ierr);
  ierr = MatSetFromOptions(Mo);CHKERRQ(ierr);
  ierr = MatSetUp(Mo);CHKERRQ(ierr);

  ierr = MatGetOwnershipRange(Mo,&Istart,&Iend);CHKERRQ(ierr);
  for (i=Istart/2;i<Iend/2;i++) {
    if (i>0) {
      j = i-1;
      ierr = MatSetValuesBlocked(Mo,1,&i,1,&j,M2t,ADD_VALUES);CHKERRQ(ierr);
      ierr = MatSetValuesBlocked(Mo,1,&i,1,&i,M3,ADD_VALUES);CHKERRQ(ierr);
    }
    if (i<nele) {
      j = i+1;
      ierr = MatSetValuesBlocked(Mo,1,&i,1,&j,M2,ADD_VALUES);CHKERRQ(ierr);
      ierr = MatSetValuesBlocked(Mo,1,&i,1,&i,M1,ADD_VALUES);CHKERRQ(ierr);
    }
  }
  ierr = MatAssemblyBegin(Mo,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(Mo,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatScale(Mo,rho*area*dlen/420);CHKERRQ(ierr);

  /* remove rows/columns from K and M corresponding to boundary conditions */
  ierr = ISCreateStride(PETSC_COMM_WORLD,Iend-Istart,Istart,1,&isf);CHKERRQ(ierr);
  bc[0] = 0; bc[1] = n;
  ierr = ISCreateGeneral(PETSC_COMM_SELF,2,bc,PETSC_USE_POINTER,&isbc);CHKERRQ(ierr);
  ierr = ISDifference(isf,isbc,&is);CHKERRQ(ierr);
  ierr = MatGetSubMatrix(Ko,is,is,MAT_INITIAL_MATRIX,&K);CHKERRQ(ierr);
  ierr = MatGetSubMatrix(Mo,is,is,MAT_INITIAL_MATRIX,&M);CHKERRQ(ierr);
  ierr = MatGetLocalSize(M,&mloc,&nloc);CHKERRQ(ierr);

  /* C is zero except for the (nele,nele)-entry */
  ierr = MatCreate(PETSC_COMM_WORLD,&C);CHKERRQ(ierr);
  ierr = MatSetSizes(C,mloc,nloc,PETSC_DECIDE,PETSC_DECIDE);CHKERRQ(ierr);
  ierr = MatSetFromOptions(C);CHKERRQ(ierr);
  ierr = MatSetUp(C);CHKERRQ(ierr);
  
  ierr = MatGetOwnershipRange(C,&Istart,&Iend);CHKERRQ(ierr);
  if (nele-1>=Istart && nele-1<Iend) { 
    ierr = MatSetValue(C,nele-1,nele-1,damp,INSERT_VALUES);CHKERRQ(ierr);
  }
  ierr = MatAssemblyBegin(C,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(C,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  
  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                Create the eigensolver and solve the problem
     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PEPCreate(PETSC_COMM_WORLD,&pep);CHKERRQ(ierr);
  A[0] = K; A[1] = C; A[2] = M;
  ierr = PEPSetOperators(pep,3,A);CHKERRQ(ierr);
  ierr = PEPSetFromOptions(pep);CHKERRQ(ierr);
  
  ierr = PetscTime(&time1); CHKERRQ(ierr);
  ierr = PEPSolve(pep);CHKERRQ(ierr);
  ierr = PetscTime(&time2); CHKERRQ(ierr);

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
                    Display solution and clean up
     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
  
  /* show detailed info unless -terse option is given by user */
  ierr = PetscOptionsHasName(NULL,"-terse",&terse);CHKERRQ(ierr);
  if (terse) {
    ierr = PEPErrorView(pep,PEP_ERROR_BACKWARD,NULL);CHKERRQ(ierr);
  } else {
    ierr = PetscViewerPushFormat(PETSC_VIEWER_STDOUT_WORLD,PETSC_VIEWER_ASCII_INFO_DETAIL);CHKERRQ(ierr);
    ierr = PEPReasonView(pep,PETSC_VIEWER_STDOUT_WORLD);CHKERRQ(ierr);
    ierr = PEPErrorView(pep,PEP_ERROR_BACKWARD,PETSC_VIEWER_STDOUT_WORLD);CHKERRQ(ierr);
    ierr = PetscViewerPopFormat(PETSC_VIEWER_STDOUT_WORLD);CHKERRQ(ierr);
  }
  ierr = PetscPrintf(PETSC_COMM_WORLD,"Time: %g\n\n\n",time2-time1);CHKERRQ(ierr);
  ierr = PEPDestroy(&pep);CHKERRQ(ierr);
  ierr = ISDestroy(&isf);CHKERRQ(ierr);
  ierr = ISDestroy(&isbc);CHKERRQ(ierr);
  ierr = ISDestroy(&is);CHKERRQ(ierr);
  ierr = MatDestroy(&M);CHKERRQ(ierr);
  ierr = MatDestroy(&C);CHKERRQ(ierr);
  ierr = MatDestroy(&K);CHKERRQ(ierr);
  ierr = MatDestroy(&Ko);CHKERRQ(ierr);
  ierr = MatDestroy(&Mo);CHKERRQ(ierr);
  ierr = SlepcFinalize();CHKERRQ(ierr);
  return 0;
}
