/*************************************************************/
/*                                                           */
/*      Factor matrix A using LAPACK subroutine CGBTRF       */
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
#define KL
#define KU

/*--- enter file location for the data of matrix A ---*/ 
#define fileA "path_to_file A"
/**************************** END CONSTANT SET UP ****************************/


/*--- global variables ---*/
int m = ROW_A;
int n = COL_A;
int kl = KL;
int ku = KU;
int ldab = 2*KL+KU+1;
  
int IPIV[min(ROW_A, COL_A)], info, i, j;
float ABT[2*(2*KL+KU+1)*COL_A];
float complex AB[(2*KL+KU+1)][COL_A], A[ROW_A][COL_A];
FILE *fptA;


/*--- external function prototype declaration ---*/
extern void OPEN_FILE();
extern void GET_DATA();
extern void PRINT_SOLUTION();



int main(){  
  /*--- message ---*/
  printf("*************************************\n");
  printf("*** Use CGBTRF to factor matrix A ***\n");
  printf("*************************************\n");
  printf("\n");
    
    
  /*--- open files that store data ---*/
  OPEN_FILE();

  
  /*--- read data ---*/
  GET_DATA();
  
  
  /*--- call lapack subroutine CGBTRF, note: ---*/  
  /*--- (1) all the arguments must be pointers ---*/ 
  /*--- (2) add an underscore to the routine name ---*/
  /*--- (3) matrices must be transformed into Fortran vector format ---*/
  CGBTRF_(&m, &n, &kl, &ku, ABT, &ldab, IPIV, &info); 

  
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

  /*--- read data of matrix A into buffer_A ---*/
  linenum = 0;
  while(fgets(line, BUFSIZ, fptA)){                                     //read the file line by line
    ptr = strtok(line, ")(, ");                                         //split line around ")(, "
    //printf("%s\n", ptr);                                              //print the first element in the line after the split
    for (j=2*(linenum-min(kl, linenum)); j<2*(linenum-min(kl, linenum))+2*(1+min(ku, n-(linenum+1))+min(kl, linenum)); j++){
      buffer_A[linenum*2*n+j] = strtof(ptr, &ptr);                      //convert ptr to float
      ptr = strtok(NULL, ")(, ");                                       //and keep splitting , search in pointer+1
    }
    linenum++;
  }
  
  /*--- get band matrix A ---*/  
  for (i=0; i<n; i++){
    for (j=0; j<n; j++){
      A[i][j] = buffer_A[2*(i*n+j)]+buffer_A[2*(i*n+j)+1]*I;
    }
  }
  
  free(buffer_A);
  
  /*--- get LAPACK band storage in AB ---*/
  for(i=0; i<n; i++){
    for(j=max(i-kl,0); j<=min(i+ku,n-1); j++){
      AB[kl+ku+i-j][j] = creal(A[i][j])+cimag(A[i][j])*I;
    }
  }
  
  /*--- transform AB into its Fortran vector format, ABT ---*/
  for (i=0; i<ldab; i++){
    for(j=0; j<n; j++){
       ABT[2*(i+ldab*j)]=creal(AB[i][j]);
       ABT[2*(i+ldab*j)+1]=cimag(AB[i][j]);
    }
  }
  
}



void PRINT_SOLUTION(){
  /*--- print factored A ---*/
  printf("Solution: \n");
  for (i=0; i<ldab; i++){
    for (j=0; j<n; j++){
      AB[i][j] = ABT[2*(i+ldab*j)]+ABT[2*(i+ldab*j)+1]*I;
    }
  }
  
  for (i=0; i<m; i++){
    for (j=0; j<n; j++){
      if (j>=max(i-kl,0) && j<n){
        printf("\t(%6.3f, %6.3f)", creal(AB[kl+ku+i-j][j]), cimag(AB[kl+ku+i-j][j]));
      }
      else{
        printf("\t\t\t");
      }
    }
    printf("\n");
  }

  /*---print info ---*/
   printf("\n\ninfo = %d\n\n", info);   
}
