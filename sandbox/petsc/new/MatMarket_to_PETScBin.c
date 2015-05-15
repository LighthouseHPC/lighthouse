/*! @file MatMarket_to_PETScBin.c
 *        @brief : This routine converts Matrix in matrix market file to PETSc Binary format
*/

/*
* For this implementation, we have used variours PETSc routines *
* For more information, check http://www.mcs.anl.gov/petsc/petsc-as/ and specifically  ksp/ex72.c & mat/ex72.c 
*/

/*PETSc Headers */
#include "petsc.h"
#include "petscmat.h"

#include "mmio.h"

int main(int argc,char **args)
{

  /*PETSc Mat Object */
  Mat         pMat;
  /* Input matrix market file and output PETSc binary file */
  char        inputFile[128],outputFile[128],buf[128];

  /* number rows, columns, non zeros etc */
  int         i,j,m,n,nnz,ierr,col,row;

  /*We compute no of nozeros per row for PETSc Mat object pre-allocation*/  
  int *nnzPtr;
  /*Maximum nonzero in nay row */
  int maxNNZperRow=0;
  /*Row number containing max non zero elements */
  int maxRowNum = 0;
  /*Just no of comments that will be ignore during successive read of file */
  int numComments=0;

  PetscScalar zero=0;

  /* This is  variable of type double */
  PetscScalar val;

  /*File handle for read and write*/
  FILE*       file;
  /*File handle for writing nonzero elements distribution per row */
  FILE 	      *fileRowDist;

   /*PETSc Viewer is used for writing PETSc Mat object in binary format */
   PetscViewer view;
  /*Just record time required for conversion */
  PetscLogDouble t1,t2,elapsed_time;

  /* MatrixMarket struct */
  MM_typecode matcode;

  /*Initialise PETSc lib */
  PetscInitialize(&argc,&args,(char *)0,PETSC_NULL);

  /* Just record time */
  //ierr = PetscGetTime(&t1); CHKERRQ(ierr);

  /*Get name of matrix market file from command line options and Open file*/
  ierr = PetscOptionsGetString(PETSC_NULL,"-fin",inputFile,127,PETSC_NULL); CHKERRQ(ierr);
  ierr = PetscFOpen(PETSC_COMM_SELF,inputFile,"r",&file); CHKERRQ(ierr);

  if (mm_read_banner(file, &matcode)) {
    PetscPrintf(PETSC_COMM_SELF, "Could not read Matrix Market banner.\n");
    exit(1);
  }

  /********************* MM_typecode query fucntions ***************************/
/* #define mm_is_matrix(typecode)	((typecode)[0]=='M') */

/* #define mm_is_sparse(typecode)	((typecode)[1]=='C') */
/* #define mm_is_coordinate(typecode)((typecode)[1]=='C') */
/* #define mm_is_dense(typecode)	((typecode)[1]=='A') */
/* #define mm_is_array(typecode)	((typecode)[1]=='A') */

/* #define mm_is_complex(typecode)	((typecode)[2]=='C') */
/* #define mm_is_real(typecode)		((typecode)[2]=='R') */
/* #define mm_is_pattern(typecode)	((typecode)[2]=='P') */
/* #define mm_is_integer(typecode) ((typecode)[2]=='I') */

/* #define mm_is_symmetric(typecode)((typecode)[3]=='S') */
/* #define mm_is_general(typecode)	((typecode)[3]=='G') */
/* #define mm_is_skew(typecode)	((typecode)[3]=='K') */
/* #define mm_is_hermitian(typecode)((typecode)[3]=='H') */
/* int mm_is_valid(MM_typecode matcode); */


  /* Do not convert pattern matrices */
  if (mm_is_pattern(matcode)) {
    printf("Pattern matrix -- skipping.\n");
    exit(0);
  }


  /* find out size of sparse matrix .... */

 
  /*Reads size of sparse matrix from matrix market file */
  int ret_code;
  if ((ret_code = mm_read_mtx_crd_size(file, &m, &n, &nnz)) !=0)
        exit(1);
  printf ("ROWS = %d, COLUMNS = %d, NO OF NON-ZEROS = %d\n",m,n,nnz);

  /* Only consider symmetric matrices */
  if (m != n) {
    printf("Nonsymmetric matrix -- skipping.\n");
    exit(0);
  }

  ierr = MatCreate(PETSC_COMM_WORLD,&pMat);CHKERRQ(ierr);
  ierr = MatSetSizes(pMat,PETSC_DECIDE,PETSC_DECIDE,m,n);CHKERRQ(ierr);
  //ierr = MatSetOption(pMat, MAT_NEW_NONZERO_ALLOCATION_ERR, PETSC_FALSE); CHKERRQ(ierr);
  if (mm_is_symmetric(matcode)) 
    ierr = MatSetOption(pMat,MAT_SYMMETRIC,PETSC_TRUE); CHKERRQ(ierr);
  ierr = MatSetFromOptions(pMat);CHKERRQ(ierr);
  ierr = MatSetUp(pMat);CHKERRQ(ierr);

 
  printf("\n MAX NONZERO FOR ANY ROW ARE : %d & ROW NUM IS : %d", maxNNZperRow, maxRowNum );
  

  /* Its important to pre-allocate memory by passing max non zero for any row in the matrix */
  //ierr = MatCreateSeqAIJ(PETSC_COMM_WORLD,m,n,maxNNZperRow,PETSC_NULL,&pMat);
  /* OR we can also pass row distribution of nozero elements for every row */
  /* ierr = MatCreateSeqAIJ(PETSC_COMM_WORLD,m,n,0,nnzPtr,&pMat);*/


  /*Now Set matrix elements values form matrix market file */
  for (i=0; i < m; i++){ 
    for (j = 0; j < n; j++) {
      if (i != j) continue;
      ierr = MatSetValues(pMat,1,&i,1,&j,&zero,INSERT_VALUES);  CHKERRQ(ierr);
    }
  }
  
  for (i=0; i<nnz; i++) 
  {
	    /*Read matrix element from matrix market file*/
	    fscanf(file,"%d %d %le\n",&row,&col,&val);

            /*In matrix market format, rows and columns starts from 1 */
	    row = row-1; col = col-1 ;
	    /* For every non zero element,insert that value at row,col position */	
	    ierr = MatSetValues(pMat,1,&row,1,&col,&val,INSERT_VALUES); CHKERRQ(ierr);
  }

  fclose(file);
  /*Matrix Read Complete */
  ierr = PetscPrintf(PETSC_COMM_SELF,"\n MATRIX READ...DONE!\n");

  /*Now assemeble the matrix */
  ierr = MatAssemblyBegin(pMat,MAT_FINAL_ASSEMBLY);
  ierr = MatAssemblyEnd(pMat,MAT_FINAL_ASSEMBLY);

  /* Now open output file for writing into PETSc Binary FOrmat*/
  ierr = PetscOptionsGetString(PETSC_NULL,"-fout",outputFile,127,PETSC_NULL);CHKERRQ(ierr);
  /*With the PETSc Viewer write output to File*/
  ierr = PetscViewerBinaryOpen(PETSC_COMM_WORLD,outputFile,FILE_MODE_WRITE,&view);CHKERRQ(ierr);
  /*Matview will dump the Mat object to binary file */
  ierr = MatView(pMat,view);CHKERRQ(ierr);
  //ierr = MatView(pMat,PETSC_VIEWER_STDOUT_WORLD);CHKERRQ(ierr);

  /* Destroy the data structure */
  ierr = PetscViewerDestroy(&view);CHKERRQ(ierr);
  ierr = MatDestroy(&pMat);CHKERRQ(ierr);

  /*Just for statistics*/
  /*
  ierr = PetscGetTime(&t2);CHKERRQ(ierr);
  elapsed_time = t2 - t1;     
  ierr = PetscPrintf(PETSC_COMM_SELF,"ELAPSE TIME: %g\n",elapsed_time);CHKERRQ(ierr);
  */

  ierr = PetscFinalize();CHKERRQ(ierr);
  return 0;
}
