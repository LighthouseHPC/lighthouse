*************************************************
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
*************************************************
	program stream2
	integer NMIN, NMAX, NTIMES, NUMSIZES
	parameter (NMIN=30,NMAX=2 000 000)
	parameter (NTIMES=10,NUMSIZES=32)
	integer NPAD
	parameter (NPAD=5)

	real*8 a(NMAX+NPAD),b(NMAX+NPAD)
	real*8 time(4,NTIMES),mysecond,scalar
	real*8 sum,start,finish
	real*8 sum0,sum1,sum2,sum3,sum4,sum5,sum6,sum7
	real*8 rate(4),besttime(4),bytes(4)
	real*8 exp,tdelta
	logical ALLTIMES
	integer i,j,k,l,M
	external mysecond

	data bytes/8,16,24,16/
	data ALLTIMES/.false./


* check timer granularity
	do i=1,min(10000,NMAX)
	    a(i) = 0.0d0
	end do
	do i=1,min(10000,NMAX)
	    a(i) = mysecond()
	end do
	tdelta = 1.d36
	do i=1,min(10000,NMAX)-1
	    if (a(i+1).ne.a(i)) then
		tdelta = min(tdelta,abs(a(i+1)-a(i)))
	    end if
	end do
	print *,'Smallest time delta is ',tdelta


	print *,'    Size  Iter     FILL      COPY     DAXPY       DOT'
* Loop over problem size
	do j=1,NUMSIZES
	    exp = log10(dble(NMIN)) + dble(j-1)/dble(NUMSIZES-1)*
     $          (log10(dble(NMAX))-log10(dble(NMIN)))
	    M = NINT(10.**exp)

* Initialize Arrays

	    do i=1,M
	        a(i) = 0.0d0
	        b(i) = 0.0d0
	    end do

	    do k=1,NTIMES
		inner = NMAX/M

	        start = mysecond()
		do l=1,inner
	            scalar = dble(k+l)
	            do i=1,M
	    	        a(i) = scalar
	            end do
	        end do
	        finish = mysecond()
	        time(1,k) = (finish-start)/dble(inner)

	        start = mysecond()
		do l=1,inner
                    a(l) = 1.0d0
	            do i=1,M
		        b(i) = a(i)
	            end do
	        end do
	        finish = mysecond()
	        time(2,k) = (finish-start)/dble(inner)

	        start = mysecond()
		do l=1,inner
                    a(l) = 1.0d0
	            do i=1,M
		        b(i) = b(i) + scalar*a(i)
	            end do
	        end do
	        finish = mysecond()
	        time(3,k) = (finish-start)/dble(inner)

	        start = mysecond()
		do l=1,inner
                    b(l) = 1.0d0
	            sum0 = 0.0d0
	            sum1 = 0.0d0
	            sum2 = 0.0d0
	            sum3 = 0.0d0
	            sum4 = 0.0d0
	            sum5 = 0.0d0
	            sum6 = 0.0d0
	            sum7 = 0.0d0
	            do i=1,M,8
		        sum0 = sum0 + a(i+0)*b(i+0)
		        sum1 = sum1 + a(i+1)*b(i+1)
		        sum2 = sum2 + a(i+2)*b(i+2)
		        sum3 = sum3 + a(i+3)*b(i+3)
		        sum4 = sum4 + a(i+4)*b(i+4)
		        sum5 = sum5 + a(i+5)*b(i+5)
		        sum6 = sum6 + a(i+6)*b(i+6)
		        sum7 = sum7 + a(i+7)*b(i+7)
	            end do
	        end do
	        sum = sum0 + sum1 + sum2 + sum3
     $              + sum4 + sum5 + sum6 + sum7
	        finish = mysecond()
	        time(4,k) = (finish-start)/dble(inner)

	    end do

	    do i=1,4 
		besttime(i) = 1.d+36
	        do k=1,NTIMES
	            besttime(i) = min(besttime(i),time(i,k))
		    if (ALLTIMES) print *,i,k,time(i,k)
	        end do
	        rate(i) = dble(M)* bytes(i)/besttime(i) / 1.d6
	    end do
	    write (*,1) M,NTIMES,rate(1),rate(2),rate(3),rate(4),
     $                  tdelta/besttime(1)
	
	    open (unit=3,form='unformatted')
	    write (3) sum
	    close (unit=3)

	end do

    1	format (1x,i8,2x,i4,1x,5(f8.1,2x))
	end
