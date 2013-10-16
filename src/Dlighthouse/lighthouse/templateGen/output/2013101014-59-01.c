/*********************************************************************/
/*                                                                   */
/*      Equilibrate matrix A using LAPACK subroutine SGBEQU          */
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
#define ROW_A
#define COL_A
#define KL
#define KU

/*--- enter file location for matrix A ---*/ 
#define fileA "path_to_file A"
/**************************** END CONSTANT SET UP ****************************/


/*--- global variables ---*/
int m = ROW_A;
int n = COL_A;
int kl = KL;
int ku = KU;
int ldab = KL+KU+1;
  
int info, i, j;
char CMACH = 's';
float SMLNUM, BIGNUM;
float amax, rowcnd, colcnd, R[ROW_A], C[COL_A], AB[(KL+KU+1)][COL_A], ABT[(KL+KU+1)*COL_A];
FILE *fptA;


/*--- external function prototype declaration ---*/
extern void OPEN_FILE();
extern void GET_DATA();
extern void PRINT_SOLUTION();
extern void SCALED_MATRIX();
extern double SLAMCH();



int main(){  
  /*--- message ---*/
  printf("******************************************\n");
  printf("*** Use SGBEQU to equilibrate matrix A ***\n");
  printf("******************************************\n");
  printf("\n");
    
    
  /*--- open files that store data ---*/
  OPEN_FILE();

  
  /*--- read data ---*/
  GET_DATA();
  
  
  /*--- compute smallest/largest safe numbers ---*/
  SMLNUM = SLAMCH(&CMACH);
  BIGNUM = 1.0/SMLNUM;
  
  printf("SMLNUM = %e\n", SMLNUM);
  printf("BIGNUM = %e\n\n", BIGNUM);
  
  
  /*--- call lapack subroutine SGBEQU, note: ---*/  
  /*--- (1) all the arguments must be pointers ---*/ 
  /*--- (2) add an underscore to the routine name ---*/
  /*--- (3) matrices must be transformed into Fortran vector format ---*/ 
  SGBEQU_(&m, &n, &kl, &ku, ABT, &ldab, R, C, &rowcnd, &colcnd, &amax, &info); 

  
  /*--- print the solution ---*/
  PRINT_SOLUTION();
  
  
  /*--- write scaled matrix if there is one ---*/
  SCALED_MATRIX();
  
  
  /*--- close files ---*/
  fclose(fptA);

  return 0;
}



void OPEN_FILE(){
  fptA = fopen(fileA,"r");
  if(fptA == NULL){
    printf("Cannot open %s.\n", fileA);    
    puts(strerror(errno));
    exit(EXIT_FAILURE);
  } 
}



void GET_DATA(){
  /*--- to call a Fortran routine from C, matrices must be transformed into their Fortran vector formats ---*/  
  /*--- read band matrix A and transform it to LAPACK band storage AB ---*/
  for(i=0; i<m; i++){
    for(j=max(i-kl,0); j<=min(i+ku,n-1); j++){
      fscanf(fptA, "%f",&AB[ku+i-j][j]);                
    }
  }    

  /*--- transform AB to ABT ---*/
  for (i=0; i<ldab; i++){
    for (j=0; j<n; j++){
      ABT[i+ldab*j]=AB[i][j];
    }
  }
  
}


void PRINT_SOLUTION(){
  printf("Solution: \n");
  printf("AMAX = %6.3e\n", amax);
  printf("ROWCND = %6.3e\n", rowcnd);
  printf("COLCND = %6.3e\n\n", colcnd);
  
  printf("Row scale factors:\n");
  for (i=0; i<m; i++){
      printf("   % 6.3e\t", R[i]);      
    }
    
  printf("\n\n");
  
  printf("Column scale factors:\n");
  for (i=0; i<m; i++){
      printf("   % 6.3e\t", C[i]);      
    }

  /*---print info ---*/
   printf("\n\ninfo = %d\n\n", info);   
}


void SCALED_MATRIX(){
  if (rowcnd>=0.1 && SMLNUM<amax && amax<BIGNUM){
    if (colcnd<0.1){
      printf("Row scaling is not needed. Column Scaled Band Matrix A =\n");
      for (j=0; j<n; j++){
        for (i=max(0,j-ku); i<=min(n-1,j+kl); i++){
          AB[ku+i-j][j] =  AB[ku+i-j][j]*C[j];
        }
      }
    }
    else
      printf("Matrix is not worth scaling.");
      exit(EXIT_SUCCESS);
  }
  else{
    if (colcnd>=0.1){
      printf("Column scaling is not needed. Row Scaled Band Matrix A =\n");
      for (j=0; j<n; j++){
        for (i=max(0,j-ku); i<=min(n-1,j+kl); i++){
          AB[ku+i-j][j] = R[i]*AB[ku+i-j][j];
        }
      }
    }
    else {
      printf("Row-and-column Scaled Band matrix A =\n");
      for (j=0; j<n; j++){
        for (i=max(0,j-ku); i<=min(n-1,j+kl); i++){
          AB[ku+i-j][j] = R[i]*AB[ku+i-j][j]*C[j];
        }
      }
    }
  }
  
  /*--- print scaled band matrix A---*/
  for (i=0; i<m; i++){
    for (j=0; j<n; j++){
      if (j>=max(i-kl,0) && j<=min(i+ku,n-1)){
        printf("\t%6.3f", AB[ku+i-j][j]);
      }
      else{
        printf("\t ");
      }
    }
    printf("\n");
  }

}

/*********************************************************************/
/*                                                                   */
/*      Equilibrate matrix A using LAPACK subroutine DGEEQU          */
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
#define ROW_A
#define COL_A

/*--- enter file location for matrix A ---*/ 
#define fileA "path_to_file A"
/**************************** END CONSTANT SET UP ****************************/


/*--- global variables ---*/
int m = ROW_A;
int n = COL_A;
int lda = ROW_A;
  
int info, i, j;
char CMACH = 's';
double SMLNUM, BIGNUM;
double amax, rowcnd, colcnd, R[ROW_A], C[COL_A], AT[ROW_A*COL_A];
FILE *fptA;


/*--- external function prototype declaration ---*/
extern void OPEN_FILE();
extern void GET_DATA();
extern void PRINT_SOLUTION();
extern void SCALED_MATRIX();
extern double DLAMCH();



int main(){  
  /*--- message ---*/
  printf("******************************************\n");
  printf("*** Use DGEEQU to equilibrate matrix A ***\n");
  printf("******************************************\n");
  printf("\n");
    
    
  /*--- open files that store data ---*/
  OPEN_FILE();

  
  /*--- read data ---*/
  GET_DATA();
  
  
  /*--- compute smallest/largest safe numbers ---*/
  SMLNUM = DLAMCH(&CMACH);
  BIGNUM = 1.0/SMLNUM;
  
  printf("SMLNUM = %e\n", SMLNUM);
  printf("BIGNUM = %e\n\n", BIGNUM);
  
  
  /*--- call lapack subroutine DGEEQU, note: ---*/  
  /*--- (1) all the arguments must be pointers ---*/ 
  /*--- (2) add an underscore to the routine name ---*/
  /*--- (3) matrices must be transformed into Fortran vector format ---*/ 
  DGEEQU_(&m, &n, AT, &lda, R, C, &rowcnd, &colcnd, &amax, &info); 

  
  /*--- print the solution ---*/
  PRINT_SOLUTION();
  
  
  /*--- write scaled matrix if there is one ---*/
  SCALED_MATRIX();
  
  
  /*--- close files ---*/
  fclose(fptA);

  return 0;
}



void OPEN_FILE(){
  fptA = fopen(fileA,"r");
  if(fptA == NULL){
    printf("Cannot open %s.\n", fileA);    
    puts(strerror(errno));
    exit(EXIT_FAILURE);
  } 
}



void GET_DATA(){
  /*--- to call a Fortran routine from C, matrices must be transformed into their Fortran vector formats ---*/
  /*--- read A and transform it to AT ---*/
  for(i=0; i<lda; i++){
    for(j=0; j<n; j++){
      fscanf(fptA, "%lf",&AT[i+lda*j]);
    }
  }
  
}


void PRINT_SOLUTION(){
  printf("Solution: \n");
  printf("AMAX = %6.3e\n", amax);
  printf("ROWCND = %6.3e\n", rowcnd);
  printf("COLCND = %6.3e\n\n", colcnd);
  
  printf("Row scale factors:\n");
  for (i=0; i<m; i++){
      printf("   % 6.3e\t", R[i]);      
    }
    
  printf("\n\n");
  
  printf("Column scale factors:\n");
  for (i=0; i<m; i++){
      printf("   % 6.3e\t", C[i]);      
    }

  /*---print info ---*/
   printf("\n\ninfo = %d\n\n", info);   
}


void SCALED_MATRIX(){
  if (rowcnd>=0.1 && SMLNUM<amax && amax<BIGNUM){
    if (colcnd<0.1){
      printf("Row scaling is not needed. Column Scaled Matrix A =\n");
      for (j=0; j<n; j++){
        for (i=0; i<m; i++){
           AT[i+m*j] =  AT[i+m*j]*C[j];
        }
      }
    }
    else
      printf("Matrix is not worth scaling.\n");
      exit(EXIT_SUCCESS);
  }
  else{
    if (colcnd>=0.1){
      printf("Column scaling is not needed. Row Scaled Matrix A =\n");
      for (j=0; j<n; j++){
        for (i=0; i<m; i++){
           AT[i+m*j] = R[i]* AT[i+m*j];
        }
      }
    }
    else {
      printf("Row-and-column Scaled Matrix A =\n");
      for (j=0; j<n; j++){
        for (i=0; i<m; i++){
           AT[i+m*j] = R[i]* AT[i+m*j]*C[j];
        }
      }
    }
  } 

  /*--- print scaled matrix A ---*/
  for (i=0; i<m; i++){
    for (j=0; j<n; j++){
      printf("   %6.3f", AT[i+m*j]);    
    }
    printf("\n");
  }
}

