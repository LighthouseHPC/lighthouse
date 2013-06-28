static char help[] = "Computes various properties of a real matrix.";

#include <petscmat.h>
//extern PetscErrorCode getDimension(Mat,PetscInt*,PetscInt*);

#undef __FUNCT__
#define __FUNCT__ "main"
int main(int argc,char **args)
{
  Mat            A;
  Vec            b;
  char           filein[PETSC_MAX_PATH_LEN],buf[PETSC_MAX_PATH_LEN];
  PetscInt       i,m,n,nnz,col,row;
  PetscErrorCode ierr;
  PetscMPIInt    size;
  PetscScalar    val;
  FILE*          file;

  PetscInitialize(&argc,&args,(char *)0,help);

  ierr = MPI_Comm_size(PETSC_COMM_WORLD,&size);CHKERRQ(ierr);
  if (size > 1) SETERRQ(PETSC_COMM_WORLD,1,"Uniprocessor Example only\n");

  /* Read in matrix and RHS */
  ierr = PetscOptionsGetString(PETSC_NULL,"-f",filein,PETSC_MAX_PATH_LEN,PETSC_NULL);CHKERRQ(ierr);
  ierr = PetscFOpen(PETSC_COMM_SELF,filein,"r",&file);CHKERRQ(ierr);

  /* process header with comments */
  char *sym = "symmetric";
  char *gen = "general";
  PetscBool isSymmetric = PETSC_FALSE;
  do {
    fgets(buf,PETSC_MAX_PATH_LEN-1,file);
    if(strstr(buf, sym) != NULL){
      isSymmetric = PETSC_TRUE;
    }
    if(strstr(buf, gen) != NULL){
      isSymmetric = PETSC_FALSE;
    }
  }while (buf[0] == '%');

  /* The first non-comment line has the matrix dimensions */
  sscanf(buf,"%d %d %d\n",&m,&n,&nnz);
  //printf ("m = %d, n = %d, nnz = %d\n",m,n,nnz);

  ierr = MatCreateSeqAIJ(PETSC_COMM_WORLD,m,n,nnz*2/m,0,&A);CHKERRQ(ierr);
  ierr = MatSetOption(A,MAT_NEW_NONZERO_ALLOCATION_ERR,PETSC_FALSE);CHKERRQ(ierr);
  ierr = VecCreate(PETSC_COMM_WORLD,&b);CHKERRQ(ierr);
  ierr = VecSetSizes(b,PETSC_DECIDE,n);CHKERRQ(ierr);
  ierr = VecSetFromOptions(b);CHKERRQ(ierr);
  ierr = VecSet(b,1);CHKERRQ(ierr);

  for (i=0; i<nnz; i++) {
    fscanf(file,"%d %d %le\n",&row,&col,(double*)&val);
    row = row-1; col = col-1 ;
    ierr = MatSetValues(A,1,&row,1,&col,&val,INSERT_VALUES);CHKERRQ(ierr);
    if(isSymmetric){
      if (row != col){
        ierr = MatSetValues(A,1,&col,1,&row,&val,INSERT_VALUES);CHKERRQ(ierr);
      }
    }
  }

  fclose(file);

  ierr = MatAssemblyBegin(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);
  ierr = MatAssemblyEnd(A,MAT_FINAL_ASSEMBLY);CHKERRQ(ierr);

  ierr = PetscPrintf(PETSC_COMM_SELF,"Matrix conversion complete.\n");CHKERRQ(ierr);
  
  /*------------------------------------*/ 

  PetscInt r=0,c=0,nz=0;
  PetscBool s;
  ierr = GetDimension(A,&r,&c);CHKERRQ(ierr);
  printf ("Dimension: m = %d, n = %d\n",r,c);
  ierr = CountNonzeros(A,&nz);CHKERRQ(ierr);
  printf ("Nonzeros: %d\n",nz);
  ierr = IsSymmetric(A,&s);CHKERRQ(ierr);
  if(s)printf ("Symmetric: True\n");
  else printf ("Symmetric: False\n");

  /*------------------------------------*/ 

  ierr = VecDestroy(&b);CHKERRQ(ierr);
  ierr = MatDestroy(&A);CHKERRQ(ierr);

  ierr = PetscFinalize();
  return 0;
}

PetscErrorCode GetDimension(Mat M, PetscInt *m, PetscInt *n)
{
  PetscErrorCode ierr;
  ierr = MatGetSize(M,m,n);CHKERRQ(ierr);
  return(0);
}

PetscErrorCode CountNonzeros(Mat M, PetscInt *nz)
{
  PetscErrorCode ierr;
  MatInfo mi;
  ierr = MatGetInfo(M,MAT_GLOBAL_SUM,&mi);CHKERRQ(ierr);
  *nz = (PetscInt)mi.nz_used;
  return(0);
}

PetscErrorCode MaxNonzerosPerRow(Mat M, PetscInt *maxnz)
{
  // PetscErrorCode ierr;
  // PetscScalar    dd;
  // PetscInt k;

  // for(k = 0; k < m; k++){
  //   ierr = VecGetValues(d,1,&k,&dd);
  //   if(dd != 1){
  //     ierr = MatSetValues(A,1,&k,1,&k,0,INSERT_VALUES);CHKERRQ(ierr);
  //     //printf ("Adding missing diagonal value at %d\n",k);
  //   }
  // }
  return(0);
}

PetscErrorCode IsSymmetric(Mat A, PetscBool *s)
{
  PetscErrorCode ierr;
  Mat Atrans;
  ierr = MatTranspose(A, MAT_INITIAL_MATRIX,&Atrans);CHKERRQ(ierr);
  ierr = MatEqual(A, Atrans, s);CHKERRQ(ierr);
  ierr = MatDestroy(&Atrans);CHKERRQ(ierr);
  return(0);
}
