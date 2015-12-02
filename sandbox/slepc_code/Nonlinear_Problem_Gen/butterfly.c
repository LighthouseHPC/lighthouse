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

   The butterfly problem is a quartic PEP with T-even structure.
*/

static char help[] = "NLEVP problem: butterfly.\n\n"
  "The command line options are:\n"
  "  -m <m>, grid size, the dimension of the matrices is n=m*m.\n"
  "  -c <array>, problem parameters, must be 10 comma-separated real values.\n\n";

#include <slepcpep.h>
#include <petsctime.h>

#define NMAT 5

#undef __FUNCT__
#define __FUNCT__ "main"
int main(int argc,char **argv)
{
  Mat            A[NMAT];         /* problem matrices */
  PEP            pep;             /* polynomial eigenproblem solver context */
  PetscInt       n,m=8,k,II,Istart,Iend,i,j;
  PetscReal      c[10] = { 0.6, 1.3, 1.3, 0.1, 0.1, 1.2, 1.0, 1.0, 1.2, 1.0 };
  PetscBool      flg;
  PetscBool      terse;
  PetscErrorCode ierr;
  PetscLogDouble time1,time2;

  SlepcInitialize(&argc,&argv,(char*)0,help);

  ierr = PetscOptionsGetInt(NULL,"-m",&m,NULL);CHKERRQ(ierr);
  n = m*m;
  k = 10;
  ierr = PetscOptionsGetRealArray(NULL,"-c",c,&k,&flg);CHKERRQ(ierr);
  if (flg && k!=10) SETERRQ1(PETSC_COMM_WORLD,1,"The number of parameters -c should be 10, you provided %D",k); 
  ierr = PetscPrintf(PETSC_COMM_WORLD,"\nButterfly problem, n=%D (m=%D)\n\n",n,m);CHKERRQ(ierr);

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                     Compute the polynomial matrices 
     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  /* initialize matrices */
  for (i=0;i<NMAT;i++) {
    ierr = MatCreate(PETSC_COMM_WORLD,&A[i]);CHKERRQ(ierr);
    ierr = MatSetSizes(A[i],PETSC_DECIDE,PETSC_DECIDE,n,n);CHKERRQ(ierr);
    ierr = MatSetFromOptions(A[i]);CHKERRQ(ierr);
    ierr = MatSetUp(A[i]);CHKERRQ(ierr);
  }
  ierr = MatGetOwnershipRange(A[0],&Istart,&Iend);CHKERRQ(ierr);

  /* A0 */
  for (II=Istart;II<Iend;II++) {
    i = II/m; j = II-i*m;
    ierr = MatSetValue(A[0],II,II,4.0*c[0]/6.0+4.0*c[1]/6.0,INSERT_VALUES);CHKERRQ(ierr);
    if (j>0) { ierr = MatSetValue(A[0],II,II-1,c[0]/6.0,INSERT_VALUES);CHKERRQ(ierr); }
    if (j<m-1) { ierr = MatSetValue(A[0],II,II+1,c[0]/6.0,INSERT_VALUES);CHKERRQ(ierr); }
    if (i>0) { ierr = MatSetValue(A[0],II,II-m,c[1]/6.0,INSERT_VALUES);CHKERRQ(ierr); }
    if (i<m-1) { ierr = MatSetValue(A[0],II,II+m,c[1]/6.0,INSERT_VALUES);CHKERRQ(ierr); }
  }

  /* A1 */
  for (II=Istart;II<Iend;II++) {
    i = II/m; j = II-i*m;
    if (j>0) { ierr = MatSetValue(A[1],II,II-1,c[2],INSERT_VALUES);CHKERRQ(ierr); }
    if (j<m-1) { ierr = MatSetValue(A[1],II,II+1,-c[2],INSERT_VALUES);CHKERRQ(ierr); }
    if (i>0) { ierr = MatSetValue(A[1],II,II-m,c[3],INSERT_VALUES);CHKERRQ(ierr); }
    if (i<m-1) { ierr = MatSetValue(A[1],II,II+m,-c[3],INSERT_VALUES);CHKERRQ(ierr); }
  }

  /* A2 */
  for (II=Istart;II<Iend;II++) {
    i = II/m; j = II-i*m;
    ierr = MatSetValue(A[2],II,II,-2.0*c[4]-2.0*c[5],INSERT_VALUES);CHKERRQ(ierr);
    if (j>0) { ierr = MatSetValue(A[2],II,II-1,c[4],INSERT_VALUES);CHKERRQ(ierr); }
    if (j<m-1) { ierr = MatSetValue(A[2],II,II+1,c[4],INSERT_VALUES);CHKERRQ(ierr); }
    if (i>0) { ierr = MatSetValue(A[2],II,II-m,c[5],INSERT_VALUES);CHKERRQ(ierr); }
    if (i<m-1) { ierr = MatSetValue(A[2],II,II+m,c[5],INSERT_VALUES);CHKERRQ(ierr); }
  }

  /* A3 */
  for (II=Istart;II<Iend;II++) {
    i = II/m; j = II-i*m;
    if (j>0) { ierr = MatSetValue(A[3],II,II-1,c[6],INSERT_VALUES);CHKERRQ(ierr); }
    if (j<m-1) { ierr = MatSetValue(A[3],II,II+1,-c[6],INSERT_VALUES);CHKERRQ(ierr); }
    if (i>0) { ierr = MatSetValue(A[3],II,II-m,c[7],INSERT_VALUES);CHKERRQ(ierr); }
    if (i<m-1) { ierr = MatSetValue(A[3],II,II+m,-c[7],INSERT_VALUES);CHKERRQ(ierr); }
  }

  /* A4 */
  for (II=Istart;II<Iend;II++) {
    i = II/m; j = II-i*m;
    ierr = MatSetValue(A[4],II,II,2.0*c[8]+2.0*c[9],INSERT_VALUES);CHKERRQ(ierr);
    if (j>0) { ierr = MatSetValue(A[4],II,II-1,-c[8],INSERT_VALUES);CHKERRQ(ierr); }
    if (j<m-1) { ierr = MatSetValue(A[4],II,II+1,-c[8],INSERT_VALUES);CHKERRQ(ierr); }
    if (i>0) { ierr = MatSetValue(A[4],II,II-m,-c[9],INSERT_VALUES);CHKERRQ(ierr); }
    if (i<m-1) { ierr = MatSetValue(A[4],II,II+m,-c[9],INSERT_VALUES);CHKERRQ(ierr); }
  }

  /* assemble matrices */
  for (i=0;i<NMAT;i++) {
    ierr = MatAssemblyBegin(A[i],MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  }
  for (i=0;i<NMAT;i++) {
    ierr = MatAssemblyEnd(A[i],MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  }

  /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
                Create the eigensolver and solve the problem
     - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

  ierr = PEPCreate(PETSC_COMM_WORLD,&pep);CHKERRQ(ierr);
  ierr = PEPSetOperators(pep,NMAT,A);CHKERRQ(ierr);
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
  for (i=0;i<NMAT;i++) {
    ierr = MatDestroy(&A[i]);CHKERRQ(ierr);
  }
  ierr = SlepcFinalize();CHKERRQ(ierr);
  return 0;
}
