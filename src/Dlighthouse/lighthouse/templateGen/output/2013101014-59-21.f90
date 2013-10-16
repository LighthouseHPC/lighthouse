        MODULE Declaration
            INTEGER                                                     :: M,N,KL,KU,LDAB,INFO, I, J
            REAL(KIND=4)                                                 :: AMAX,COLCND,ROWCND, SMLNUM, BIGNUM
            REAL(kind=8)                                                :: SLAMCH
            REAL(KIND=4), DIMENSION(:), ALLOCATABLE                      :: C,R
            REAL(KIND=4), DIMENSION(:,:), ALLOCATABLE                :: AB
            
            EXTERNAL SLAMCH
        END MODULE Declaration
                

        PROGRAM TEMP_sgbequ
            USE Declaration
            IMPLICIT NONE
            
            !--- message ---!
            PRINT *, "********************************************"
            PRINT *, "*** Use sgbequ to equilibrate matrix A. ***"
            PRINT *, "********************************************"
            PRINT *,
 
 
            !--- open files that store data ---!
            CALL OPEN_FILES


            !--- allocate storage ---!
            CALL DIMNS_ASSIGNMENT

            
            !--- read data ---!
            CALL GET_DATA
        
        
            !--- compute smallest/largest safe numbers ---!       
            SMLNUM = SLAMCH('s')
            BIGNUM = 1.0/SMLNUM
            
            
            !--- call lapack subroutine sgbequ ---!
            CALL sgbequ ( M, N, KL, KU, AB, LDAB, R, C, ROWCND, COLCND, AMAX, INFO )

            
            !--- write the solution ---!
            CALL PRINT_SOLUTION
            
            
            !--- print scaled matrix ---!
            CALL MATRIX_SCALING
            
             
            !--- write INFO ---!
            WRITE(*,*)
            WRITE(*, *) "INFO = ", INFO
            
            DEALLOCATE(AB,C,R)
        END PROGRAM TEMP_sgbequ
        


        
        SUBROUTINE OPEN_FILES
            !--- obtain the dada of the matrix A ---!
            OPEN (unit=11, file='file_name_for_data_A', status='old')
            
        END SUBROUTINE OPEN_FILES
        

	SUBROUTINE DIMNS_ASSIGNMENT
	    USE Declaration
	    !--- obtain inputs ---!
	    WRITE(*, '(A)', ADVANCE = 'NO') "Input number of columns in matrix A. N >= 0. N = "
	    READ *, N 
            WRITE(*, '(A)', ADVANCE = 'NO') "Input number of rows in matrix A. M >= 0. M = "
            READ *, M
	    WRITE(*, '(A)', ADVANCE = 'NO') "Input the number of superdiagonals within the band of A. KU >= 0. KU = "
            READ *, KU
	    WRITE(*, '(A)', ADVANCE = 'NO') "Input the number of subdiagonals within the band of A. KL >= 0. KL = "
            READ *, KL

	    LDAB = KL+KU+1

	    !--- allocate arrays ---!
	    ALLOCATE(AB(LDAB,N))
	    ALLOCATE(C(N))
	    ALLOCATE(R(M))
	END SUBROUTINE DIMNS_ASSIGNMENT


	SUBROUTINE GET_DATA
	    USE Declaration
	    !--- read data from files ---!
	    READ(11, *) ((AB(KU+1+I-J,J),J=MAX(I-KL,1),MIN(I+KU,N)),I=1,N)

	END SUBROUTINE GET_DATA


        SUBROUTINE PRINT_SOLUTION
            USE Declaration

            WRITE(*,*)
            WRITE(*,33300) 'AMAX = ', AMAX
            WRITE(*,33300) 'ROWCND = ', ROWCND
            WRITE(*,33300) 'COLCND = ', COLCND
            WRITE(*,*)
            WRITE(*,*) 'Row scale factors:'
            WRITE(*, 44400) (R(I), I=1,M)
            WRITE(*,*)
            WRITE(*,*) 'Column scale factors:'           
            WRITE(*, 44400) (C(J), J=1,N)
            WRITE(*,*)
            
33300       FORMAT(1X,3(A,1P,ES15.2))

44400       FORMAT(11(:,1X, ES15.2))
        END SUBROUTINE PRINT_SOLUTION
        
        
        SUBROUTINE MATRIX_SCALING
            USE Declaration
            
            !--- determin type of scaling ---!
            IF (ROWCON>=0.1 .AND. SMLNUM<AMAX .AND. AMAX<BIGNUM) THEN
                IF (COLCON<0.1) THEN
                    WRITE(*,*) 'Row scaling is not needed. Column Scaled Matrix A = '
                    DO J=1,N
                        DO I=MAX(1,J-KU), MIN(N,J+KL)
                            AB(KU+1+I-J, J) = AB(KU+1+I-J, J)*C(J)
                        END DO
                    END DO
                ELSE
                    WRITE(*,*) 'Matrix is not worth scaling.'
                END IF
            ELSE
                IF (COLCON>=0.1) THEN
                    WRITE(*,*) 'Column scaling is not needed. Row Scaled Matrix A = '
                    DO J=1,N
                        DO I=MAX(1,J-KU), MIN(N,J+KL)
                            AB(KU+1+I-J, J) = R(I)*AB(KU+1+I-J, J)
                        END DO
                    END DO
                ELSE
                    WRITE(*,*) 'Row-and-column Scaled Matrix A = '
                    DO J=1,N
                        DO I=MAX(1,J-KU), MIN(N,J+KL)
                            AB(KU+1+I-J, J) = R(I)*AB(KU+1+I-J, J)*C(J)
                        END DO
                    END DO
                END IF
            END IF
                  
            
            DO I=1,M     
                WRITE (*,11100) (AB(KU+1+I-J, J), J=MAX(I-KL,1),MIN(I+KU,N))
            END DO
            
            !--- real ---!
11100       FORMAT(11(:,1X, F8.4))            

            !--- complex ---!    
22200       FORMAT( 11(:,1X,'(',F12.6,',',F12.6,')') )
        END SUBROUTINE MATRIX_SCALING
        MODULE Declaration
            INTEGER                                                     :: M,N,LDA,INFO, I, J
            REAL(KIND=8)                                                 :: AMAX,COLCND,ROWCND, SMLNUM, BIGNUM
            REAL(kind=8)                                                :: DLAMCH
            REAL(KIND=8), DIMENSION(:), ALLOCATABLE                      :: C,R
            REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE                :: A
            
            EXTERNAL DLAMCH
        END MODULE Declaration
                

        PROGRAM TEMP_dgeequ
            USE Declaration
            IMPLICIT NONE
            
            !--- message ---!
            PRINT *, "********************************************"
            PRINT *, "*** Use dgeequ to equilibrate matrix A. ***"
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
            
            
            !--- call lapack subroutine dgeequ ---!
            CALL dgeequ ( M, N, A, LDA, R, C, ROWCND, COLCND, AMAX, INFO )

            
            !--- write the solution ---!
            CALL PRINT_SOLUTION
            
            
            !--- print scaled matrix ---!
            CALL MATRIX_SCALING
            
             
            !--- write INFO ---!
            WRITE(*,*)
            WRITE(*, *) "INFO = ", INFO
            
            DEALLOCATE(A,C,R)
        END PROGRAM TEMP_dgeequ
        


        
        SUBROUTINE OPEN_FILES
            !--- obtain the dada of the matrix A ---!
            OPEN (unit=11, file='file_name_for_data_A', status='old')
            
        END SUBROUTINE OPEN_FILES
        

	SUBROUTINE DIMNS_ASSIGNMENT
	    USE Declaration
	    !--- obtain inputs ---!
	    WRITE(*, '(A)', ADVANCE = 'NO') "Input number of columns in matrix A. N >= 0. N = "
	    READ *, N 
            WRITE(*, '(A)', ADVANCE = 'NO') "Input number of rows in matrix A. M >= 0. M = "
            READ *, M

	    LDA = max(1,M)

	    !--- allocate arrays ---!
	    ALLOCATE(A(LDA,N))
	    ALLOCATE(C(N))
	    ALLOCATE(R(M))
	END SUBROUTINE DIMNS_ASSIGNMENT


	SUBROUTINE GET_DATA
	    USE Declaration
	    !--- read data from files ---!
	    READ(11, *) ((A(I,J),J=1,N),I=1,N)

	END SUBROUTINE GET_DATA


        SUBROUTINE PRINT_SOLUTION
            USE Declaration

            WRITE(*,*)
            WRITE(*,33300) 'AMAX = ', AMAX
            WRITE(*,33300) 'ROWCND = ', ROWCND
            WRITE(*,33300) 'COLCND = ', COLCND
            WRITE(*,*)
            WRITE(*,*) 'Row scale factors:'
            WRITE(*, 44400) (R(I), I=1,M)
            WRITE(*,*)
            WRITE(*,*) 'Column scale factors:'           
            WRITE(*, 44400) (C(J), J=1,N)
            WRITE(*,*)
            
33300       FORMAT(1X,3(A,1P,ES15.2))

44400       FORMAT(11(:,1X, ES15.2))
        END SUBROUTINE PRINT_SOLUTION
        
        
        SUBROUTINE MATRIX_SCALING
            USE Declaration
            
            !--- determin type of scaling ---!
            IF (ROWCON>=0.1 .AND. SMLNUM<AMAX .AND. AMAX<BIGNUM) THEN
                IF (COLCON<0.1) THEN
                    WRITE(*,*) 'Row scaling is not needed. Column Scaled Matrix A = '
                    DO J=1,N
                        DO I=1,M
                            A(I,J) = A(I,J)*C(J)
                        END DO
                    END DO
                ELSE
                    WRITE(*,*) 'Matrix is not worth scaling.'
                END IF
            ELSE
                IF (COLCON>=0.1) THEN
                    WRITE(*,*) 'Column scaling is not needed. Row Scaled Matrix A = '
                    DO J=1,N
                        DO I=1,M
                            A(I,J) = R(I)*A(I,J)
                        END DO
                    END DO
                ELSE
                    WRITE(*,*) 'Row-and-column Scaled Matrix A = '
                    DO J=1,N
                        DO I=1,M
                            A(I,J) = R(I)*A(I,J)*C(J)
                        END DO
                    END DO
                END IF
            END IF
                  
            !--- print scaled matrix ---!
            DO I = 1,M      
                WRITE (*,11100) (A(I,J), J=1,N)
            END DO
            
            !--- real ---!
11100       FORMAT(11(:,1X, F8.4))            

            !--- complex ---!    
22200       FORMAT( 11(:,1X,'(',F12.6,',',F12.6,')') )

        END SUBROUTINE MATRIX_SCALING
