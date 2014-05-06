/*********************************************************************/
/*                                                                   */
/*   Solve the matrix equation A*x=B using LAPACK subroutine SGESV   */
/*                                                                   */
/*                   -- Created by the Lighthouse Development Team   */
/*                                                                   */
/*********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <complex.h>

#define min(a, b) (((a) < (b)) ? (a) : (b)) 
#define max(a, b) (((a) > (b)) ? (a) : (b))

/**************************** BEGIN CONSTANT SET UP ****************************/
/*--- input matrix properties ---*/
#define SIZE
#define ROW_B
#define COL_B

/*--- enter file locations for (1) matrix A and (2) matrix B ---*/ 
#define fileA "path_to_file A"
#define fileB "path_to_file B"
/**************************** END CONSTANT SET UP ****************************/


/*--- global variables ---*/
int n = SIZE;
int lda = SIZE;
int nrhs = COL_B;
int ldb = ROW_B;
  
int IPIV[SIZE], info, i, j;
float AT[SIZE*SIZE], BT[ROW_B*COL_B];
FILE *fptA, *fptB;


/*--- external function prototype declaration ---*/
extern void OPEN_FILE();
extern void GET_DATA();
extern void PRINT_SOLUTION();



int main(){  
  /*--- message ---*/
  printf("*********************************\n");
  printf("*** Use SGESV to solve Ax = B ***\n");
  printf("*********************************\n");
  printf("\n");
    
    
  /*--- open files that store data ---*/
  OPEN_FILE();

  
  /*--- read data ---*/
  GET_DATA();
  
  
  /*--- call lapack subroutine SGESV, note: ---*/  
  /*--- (1) all the arguments must be pointers ---*/ 
  /*--- (2) add an underscore to the routine name ---*/
  /*--- (3) matrices must be transformed into their Fortran vector formats ---*/ 
  SGESV_(&n, &nrhs, AT, &lda, IPIV, BT, &ldb, &info); 

  
  /*--- print the solution ---*/
  PRINT_SOLUTION();
  
  
  /*--- close files ---*/
  fclose(fptA);
  fclose(fptB);

  return 0;
}



void OPEN_FILE(){
  fptA = fopen(fileA,"r");
  if(fptA == NULL){
    printf("Cannot open %s.\n", fileA);    
    puts(strerror(errno));
    exit(EXIT_FAILURE);
  }

  fptB = fopen(fileB,"r");
  if(fptB == NULL){
    printf("Cannot open %s.\n", fileB);    
    puts(strerror(errno));
    exit(EXIT_FAILURE);
  } 
}



void GET_DATA(){
  /*--- to call a Fortran routine from C, matrices must be transformed into their Fortran vector formats ---*/
  /*--- read A and transform it to AT ---*/
  for(i=0; i<lda; i++){
    for(j=0; j<n; j++){
      fscanf(fptA, "%f",&AT[i+lda*j]);
    }
  }
  
  /*--- read B and transform it to BT ---*/
  for(i=0; i <ldb; i++){
    for (j=0; j<nrhs; j++){
      fscanf(fptB, "%f",&BT[i+ldb*j]);
    }
  }
}



void PRINT_SOLUTION(){
  /*--- print vector x (stored in B) ---*/
  printf("Solution: \n");
  for (i=0; i<n; i++){
    for (j=0; j<nrhs; j++){
      printf("\t%6.3f", BT[i+n*j]);    
    }
    printf("\n");
  }

  /*---print info ---*/
   printf("\ninfo = %d", info);   
}

