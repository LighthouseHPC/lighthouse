        MODULE Declaration
            INTEGER                                                     :: M,N,KL,KU,LDAB,INFO, I, J
            INTEGER, DIMENSION(:), ALLOCATABLE                          :: IPIV
            COMPLEX(KIND=4), DIMENSION(:,:), ALLOCATABLE                :: AB

        END MODULE Declaration
                

        PROGRAM TEMP_cgbtrf
            USE Declaration
            IMPLICIT NONE
            
            !--- message ---!
            PRINT *, "*************************************"
            PRINT *, "*** Use cgbtrf to factor matrix A ***"
            PRINT *, "*************************************"
            PRINT *,
 
 
            !--- open files that store data ---!
            CALL OPEN_FILES


            !--- allocate storage ---!
            CALL DIMNS_ASSIGNMENT

            
            !--- read data ---!
            CALL GET_DATA

            
            !--- call lapack subroutine cgbtrf ---!
            CALL cgbtrf ( M, N, KL, KU, AB, LDAB, IPIV, INFO )

            
            !--- write the solution ---!
            CALL PRINT_SOLUTION
            
 
            !--- write INFO ---!
            WRITE(*, *) "info = ", INFO
            
            DEALLOCATE(AB,IPIV)
        END PROGRAM TEMP_cgbtrf
        


        
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

	    LDAB = 2*KL+KU+1

	    !--- allocate arrays ---!
	    ALLOCATE(AB(LDAB, N))
	    ALLOCATE(IPIV(min(M,N)))
	END SUBROUTINE DIMNS_ASSIGNMENT


	SUBROUTINE GET_DATA
	    USE Declaration
	    !--- read data from files ---!
	    READ(11, *) ((AB(KL+KU+1+I-J,J),J=MAX(I-KL,1),MIN(I+KU,N)),I=1,N)

	END SUBROUTINE GET_DATA


        SUBROUTINE PRINT_SOLUTION
            USE Declaration

            WRITE(*,*)
            WRITE(*,*) 'Factored Matrix: '
            DO I=1,N
                WRITE(*,22200) (AB(KL+KU+1+I-J,J),J=MAX(I-KL,1),MIN(I+KU,N))
            END DO
            WRITE(*,*)
            
            !--- real ---!
11100       FORMAT(11(:,1X, F8.4))            

            !--- complex ---!    
22200       FORMAT( 11(:,1X,'(',F12.6,',',F12.6,')') )
        END SUBROUTINE PRINT_SOLUTION
