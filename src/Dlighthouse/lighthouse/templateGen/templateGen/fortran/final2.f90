        PROGRAM TEMP_dpbsv 
            IMPLICIT NONE
            
            INTEGER                                                     :: N, KD, NRHS, LDAB, LDB, INFO
            CHARACTER(LEN=1)                                            :: UPLO
            REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE                :: AB, B
            
            PRINT *, "*********************************"
            PRINT *, "*** Use dpbsv to solve Ax = B ***"
            PRINT *, "*********************************"
            PRINT *,
 
            !--- obtain the dada of the square matrix A ---!
            OPEN (unit=11, file='file_name_for_data_A', status='old')
            
            !--- obtain the data of right hand side B ---!
            OPEN (unit=99, file='file_name_for_data_B', status='old')


!--- obtain inputs ---!
PRINT *, 'Input leading dimension of B, LDB = '
READ *, LDB 
PRINT *, 'Input leading dimension of AB, LDAB = '
READ *, LDAB 
PRINT *, 'Input the number of superdiagonals(or subdiagonals) of the matrix A if UPLO = 'U'(or UPLO = 'L'). KD = '
READ *, KD 
PRINT *, 'Input 'U' for upper triangular matrix or 'L' for lower triangular matrix. UPLO = '
READ *, UPLO 
PRINT *, 'Input the order of your square matrix. N = '
READ *, N 
PRINT *, 'Input number of columns in B. NRHS = '
READ *, NRHS 


!--- allocate matrix/array ---!
ALLOCATE(B(LDB,NRHS)) 
ALLOCATE(AB(LDAB,N)) 


!--- read data from file for B ---!
READ(99,*) ((B(I,J),J=1,NRHS),I=1,N)

!--- read data from file for AB ---!
IF (UPLO == 'U') THEN
    READ(11, *) ((AB(KD+1+I-J,J),J=I,MIN(N,I+KD)),I=1,N)
ELSE IF (UPLO == 'L') THEN
    READ(11, *) ((AB(1+I-J,J),J=MAX(1,I-KD),I),I=1,N)
END IF


!--- call lapack subroutine dpbsv ---!
CALL DPBSV( UPLO, N, KD, NRHS, AB, LDAB, B, LDB, INFO )


            !--- write the solution ---!
            !--- Note: the solution is returned in B and A has been changed ---!  
            WRITE(*,*)
            WRITE(*,*) 'SOLUTION: '
            DO I = 1, N
                WRITE(*,11100) ( B( I, J ), J = 1, NRHS )
            END DO
            WRITE(*,*)
                
            WRITE(*, *) "info = ", info
            WRITE(*, *) "Pivot indices = ", ipiv
            
            !--- real ---!
11100       FORMAT(11(:,1X, F6.2))            

            !--- complex ---!    
22200       FORMAT( 11(:,1X,'(',F12.6,',',F12.6,')') )
            
            DEALLOCATE(a,b,ipiv)
        END PROGRAM TEMP_dpbsv
