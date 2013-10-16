/*************************************************************/
/*                                                           */
/*      Factor matrix A using LAPACK subroutine CGETRF       */
/*                                                           */
/*       -- Created by the Lighthouse Development Team       */
/*                                                           */
/*************************************************************/

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

/*--- enter file location for the data of matrix A ---*/ 
#define fileA "path_to_file A"
/**************************** END CONSTANT SET UP ****************************/


/*--- global variables ---*/
int m = ROW_A;
int n = COL_A;
int lda = ROW_A;
  
int IPIV[min(ROW_A,COL_A)], info, i, j;
float AT[2*ROW_A*COL_A];
float complex A[ROW_A][COL_A];
FILE *fptA;


/*--- external function prototype declaration ---*/
extern void OPEN_FILE();
extern void GET_DATA();
extern void PRINT_SOLUTION();



int main(){  
  /*--- message ---*/
  printf("*************************************\n");
  printf("*** Use CGETRF to factor matrix A ***\n");
  printf("*************************************\n");
  printf("\n");
    
    
  /*--- open files that store data ---*/
  OPEN_FILE();

  
  /*--- read data ---*/
  GET_DATA();
  
  
  /*--- call lapack subroutine CGETRF, note: ---*/  
  /*--- (1) all the arguments must be pointers ---*/ 
  /*--- (2) add an underscore to the routine name ---*/
  /*--- (3) matrices must be transformed into Fortran vector format ---*/
  CGETRF_(&m, &n, AT, &lda, IPIV, &info); 

  
  /*--- print the solution ---*/
  PRINT_SOLUTION();
  
  
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
  int linenum;
  char *ptr, line[BUFSIZ];
  float *buffer_A;

  /*--- allocate temporary array buffer_A ---*/ 
  buffer_A = (float*)malloc (sizeof (float)*(2*n*n));
  
  /*--- read matrix into buffer_A ---*/
  linenum = 0;
  while(fgets(line, BUFSIZ, fptA)){                                     //read the file line by line
    ptr = strtok(line, ")(, ");                                         //split line around ")(, "
    //printf("%s\n", ptr);                                              //print the first element in the line after the split
    for (i=0; i<2*n; i++){
      buffer_A[linenum*2*n+i] = strtof(ptr, &ptr);                      //convert ptr to float
      ptr = strtok(NULL, ")(, ");                                       //and keep splitting , search in pointer+1
    }
    linenum++;
  }
  
  
  /*--- get A ---*/
  for (i=0; i<lda; i++){
    for (j=0; j<n; j++){
      A[i][j] = buffer_A[2*(i*n+j)]+buffer_A[2*(i*n+j)+1]*I;
    }
  }
    
  free(buffer_A);
  
  /*--- transform A to its Fortran vector format AT ---*/
  for (i=0; i<lda; i++){
    for(j=0; j<n; j++){
       AT[2*(i+lda*j)]=creal(A[i][j]);
       AT[2*(i+lda*j)+1]=cimag(A[i][j]);
    }
  }
  
}



void PRINT_SOLUTION(){
  /*--- print factored A ---*/
  printf("Solution: \n");
  for (i=0; i<lda; i++){
    for (j=0; j<n; j++){
      printf("\t(%6.3f, %6.3f)", AT[2*(i+lda*j)], AT[2*(i+lda*j)+1]);    
    }
    printf("\n");
  }

  /*---print info ---*/
   printf("\n\ninfo = %d\n\n", info);   
}
