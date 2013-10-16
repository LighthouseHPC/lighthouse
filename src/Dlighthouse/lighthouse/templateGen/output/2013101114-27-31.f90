        MODULE Declaration
            INTEGER                                                     :: N,LDA,INFO, I, J
            CHARACTER(LEN=1)                                            :: UPLO
            REAL(KIND=8)                                                 :: AMAX,SCOND, SMLNUM, BIGNUM
            REAL(kind=8)                                                :: DLAMCH
            REAL(KIND=8), DIMENSION(:), ALLOCATABLE                      :: S
            REAL(KIND=8), DIMENSION(:), ALLOCATABLE                  :: WORK
            REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE                :: A
            
            EXTERNAL DLAMCH
        END MODULE Declaration
                

        PROGRAM TEMP_dsyequb
            USE Declaration
            IMPLICIT NONE
            
            !--- message ---!
            PRINT *, "********************************************"
            PRINT *, "*** Use dsyequb to equilibrate matrix A. ***"
            PRINT *, "********************************************"
            PRINT *,
 
 
            !--- open files that store data ---!
            CALL OPEN_FILES


            !--- allocate storage ---!
            CALL DIMNS_ASSIGNMENT

            
            !--- read data ---!
            CALL GET_DATA
        
        
            !--- compute smallest/largest safe numbers ---!       
            SMLNUM = DLAMCH('s')
            BIGNUM = 1.0/SMLNUM
            
            
            !--- call lapack subroutine dsyequb ---!
            CALL dsyequb ( UPLO, N, A, LDA, S, SCOND, AMAX, WORK, INFO )

            
            !--- write the solution ---!
            CALL PRINT_SOLUTION
            
            
            !--- print scaled matrix ---!
            CALL MATRIX_SCALING
            
             
            !--- write INFO ---!
            WRITE(*,*)
            WRITE(*, *) "INFO = ", INFO
            
            DEALLOCATE(A,WORK,S)
        END PROGRAM TEMP_dsyequb
        


        
        SUBROUTINE OPEN_FILES
            !--- obtain the dada of the matrix A ---!
            OPEN (unit=11, file='file_name_for_data_A', status='old')
            
        END SUBROUTINE OPEN_FILES
        

	SUBROUTINE DIMNS_ASSIGNMENT
	    USE Declaration
	    !--- obtain inputs ---!
            WRITE(*, '(A)', ADVANCE = 'NO') "Input 'U' for upper triangular matrix or 'L' for lower triangular matrix. UPLO = "
            READ *, UPLO
	    WRITE(*, '(A)', ADVANCE = 'NO') "Input number of columns in matrix A. N >= 0. N = "
	    READ *, N 

	    LDA = max(1,N)

	    !--- allocate arrays ---!
	    ALLOCATE(A(LDA,N))
	    ALLOCATE(WORK(3*N))
	    ALLOCATE(S(N))
	END SUBROUTINE DIMNS_ASSIGNMENT


	SUBROUTINE GET_DATA
	    USE Declaration
	    !--- read data from files ---!
	    IF (UPLO == 'U') THEN
	        READ(11, *) ((A(I,J),J=I,N),I=1,N)
	    ELSE IF (UPLO == 'L') THEN
	        READ(11, *) ((A(I,J),J=1,I),I=1,N)
	    END IF

	END SUBROUTINE GET_DATA


        SUBROUTINE PRINT_SOLUTION
            USE Declaration

            WRITE(*,*)
            WRITE(*,33300) 'AMAX = ', AMAX
            WRITE(*,33300) 'SCOND = ', SCOND
            WRITE(*,*)
            WRITE(*,*) 'Scale factors:'
            WRITE(*, 44400) (S(I), I=1,N)
            WRITE(*,*)
            
33300       FORMAT(1X,3(A,1P,ES15.2))

44400       FORMAT(11(:,1X, ES15.2))
        END SUBROUTINE PRINT_SOLUTION
        
        
        SUBROUTINE MATRIX_SCALING
            USE Declaration
            
            !--- determin type of scaling ---!
            IF (SCOND>=0.1 .AND. SMLNUM<AMAX .AND. AMAX<BIGNUM) THEN
                WRITE(*,*) 'Matrix is not worth scaling.'
            ELSE
                WRITE(*,*) 'Scaled Matrix A = '
                IF (UPLO =='U') THEN
                    DO J=1,N
                        DO I=1,J
                            A(I,J) = S(I)*A(I,J)*S(J)
                        END DO
                    END DO
                ELSE IF (UPLO == 'L') THEN
                    DO J=1,N
                        DO I=J,N
                            A(I,J) = S(I)*A(I,J)*S(J)
                        END DO
                    END DO
                END IF
            END IF
                  
            
            DO I=1,N
                IF (UPLO=='U') THEN
                    WRITE (*,11100) (A(I,J), J=I,N)
                ELSE IF (UPLO=='L') THEN
                    WRITE (*,11100) (A(I,J), J=1,I)
                END IF
            END DO
            
            !--- real ---!
11100       FORMAT(11(:,1X, F8.4))            

            !--- complex ---!    
22200       FORMAT( 11(:,1X,'(',F12.6,',',F12.6,')') )
        END SUBROUTINE MATRIX_SCALING
