/************************************************
* Program:  STREAM2                             *
* Revision: 0.1, 99.10.26                       *
* Author:   John McCalpin                       *
*           john@mccalpin.com                   *
*************************************************
*-----------------------------------------------------------------------
* Copyright 1991-2003: John D. McCalpin
*-----------------------------------------------------------------------
* License:
*  1. You are free to use this program and/or to redistribute
*     this program.
*  2. You are free to modify this program for your own use,
*     including commercial use, subject to the publication
*     restrictions in item 3.
*  3. You are free to publish results obtained from running this
*     program, or from works that you derive from this program,
*     with the following limitations:
*     3a. In order to be referred to as "STREAM2 benchmark results",
*         published results must be in conformance to the STREAM
*         Run Rules, (briefly reviewed below) published at
*         http://www.cs.virginia.edu/stream/ref.html
*         and incorporated herein by reference.
*         As the copyright holder, John McCalpin retains the
*         right to determine conformity with the Run Rules.
*     3b. Results based on modified source code or on runs not in
*         accordance with the STREAM Run Rules must be clearly
*         labelled whenever they are published.  Examples of
*         proper labelling include:
*         "tuned STREAM2 benchmark results"
*         "based on a variant of the STREAM2 benchmark code"
*         Other comparable, clear and reasonable labelling is
*         acceptable.
*     3c. Submission of results to the STREAM benchmark web site
*         is encouraged, but not required.
*  4. Use of this program or creation of derived works based on this
*     program constitutes acceptance of these licensing restrictions.
*  5. Absolutely no warranty is expressed or implied.
*-----------------------------------------------------------------------
*************************************************
* This program measures sustained bandwidth     *
* using four computational kernels:             *
*                                               *
*       FILL:   a(i) = 0                        *
*       COPY:   a(i) = b(i)                     *
*       DAXPY:  a(i) = a(i) + q*b(i)            *
*       DOT:    sum += a(i) * b(i)              *
*                                               *
* Results are presented in MB/s, assuming       *
*   8 Bytes per iteration for FILL,             *
*  16 Bytes per iteration for COPY & DOT, and   *
*  24 Bytes per iteration for DAXPY             *
************************************************/
//Conversion to C by Ian Karlin

#include "stream2.h"
#include "mysecond.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

double** stream2(int points, int* sizes) {
  int nmin = 30; 
  int nmax = 2000000;
  int ntimes = 10;
  int numsizes = points;
  int npad = 5;
  double *a;
  double *b;
  double time[4][ntimes];
  double scalar;
  double sum;
  double sum0 = 0;
  double sum1 = 0;
  double sum2 = 0;
  double sum3 = 0;
  double sum4 = 0;
  double sum5 = 0;
  double sum6 = 0;
  double sum7 = 0;
  double start;
  double finish;
  double rate[4];
  double besttime[4];
  double bytes[4] = {8, 16, 24, 16};
  double exp;
  double tdelta;
  int alltimes = 0;
  int i, j, k, l, m;
  int inner;
  double **bandwidths;

  a = malloc((npad+nmax)*sizeof(double)*2);
  b = malloc((npad+nmax)*sizeof(double));
  bandwidths = malloc(4*sizeof(double*));
  for(i = 0; i < 4; i++)
    bandwidths[i] = malloc(points*sizeof(double));
//check timer for granularity
  for(i = 0; i < 10000; i++)
    a[i] = 0.0;
  for(i = 0; i < 10000; i++)
    a[i] = mysecond();
  tdelta = 1000000000;
  for(i = 1; i < 9999; i++)
    if(a[i+1] != a[i])
      if(tdelta > abs(a[i-1] - a[i]))
        tdelta = abs(a[i-1] - a[i]);
  printf("Smallest time delta is %lf\n", tdelta);
  printf("    Size  Iter      FILL      COPY     DAXPY       DOT\n");

//Loop over problem size
  for(j = 0; j < numsizes; j++) {
    exp = log10((double)nmin) + (double)(j-1)/(double)(numsizes-1) * (log10((double)nmax) - log10((double)nmin));
    m = lround(pow(10, exp));
   
//Initialize Arrays
    for(i = 0; i < m; i++) {
      a[i] = 0.0;
      b[i] = 0.0;
    }

    for(k = 0; k < ntimes; k++) {
      inner = nmax/m;
      start = mysecond();
      for(l = 0; l < inner; l++) {
        scalar = (double)(k+l);
	for(i = 0; i < m*2; i++)
	  a[i] = scalar;
      }
      finish = mysecond();
      time[0][k] = (finish-start)/(double)inner;
      
      start = mysecond();
      for(l = 0; l < inner*2; l+=2) {
        a[l] = 1.0;
	for(i = 0; i < m*2; i+=2)
	  a[i+1] = a[i];
      }
      finish = mysecond();
      time[1][k] = (finish-start)/(double)inner;

      start = mysecond();
      for(l = 0; l < inner*2; l+=2) {
	a[l] = 1.0;
	for(i = 0; i < m*2; i+=2)
	  a[i+1] = a[i+1] + scalar*a[i];
      }
      finish = mysecond();
      time[2][k] = (finish-start)/(double)inner;

      start = mysecond();
      for(l = 0; l < inner*2; l+=2) 
	a[l+1] = 1.0;
      for(l = 0; l < inner; l++) {
	sum0 = 0.0;
	sum1 = 0.0;
	sum2 = 0.0;
	sum3 = 0.0;
	for(i = 0; i < m*2; i += 4) {
          sum0 += a[i]*a[i+1];
          sum1 += a[i + 2]*a[i + 3];
          sum2 += a[i + 4]*a[i + 5];
          sum3 += a[i + 6]*a[i + 7];
        }
      }
      sum = sum0 + sum1 + sum2 + sum3;
      finish = mysecond();
      time[3][k] = (finish-start)/(double)inner;
    }  
      for(i = 0; i < 4; i++) {
	besttime[i] = 1000000000.0;
        for(k = 0; k < ntimes; k++) {
          if(time[i][k] < besttime[i])
	    besttime[i] = time[i][k];
	  //if(alltimes)
//	    printf("%d %d %lf\n)", i, k, time[i][k]);
	}
       
        //printf("%d %d %lf\n", m, ntimes, besttime[i]);
        rate[i] = (double)m * (double)bytes[i]/(double)besttime[i]/1000000.0;
      }
      fprintf(stderr, "%d %d %d %lf %lf %lf %lf %lf\n", j, m, ntimes, rate[0], rate[1], rate[2], rate[3], tdelta/besttime[1]);
//      printf("%d %d %lf\n", m, ntimes, rate[0]);
      bandwidths[0][j] = rate[0];
      bandwidths[1][j] = rate[1];
      bandwidths[2][j] = rate[2];
      bandwidths[3][j] = rate[3];
      sizes[j] = m;
  }
  fprintf(stderr, "beat the optimizer: %lf\n", sum);
  return bandwidths;
}
