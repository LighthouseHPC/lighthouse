        MODULE Declaration
            INTEGER                                                     :: N,NRHS,LDA,LDB,INFO, I, J
            INTEGER, DIMENSION(:), ALLOCATABLE                          :: IPIV
            REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE                :: A,B
            
        END MODULE Declaration
                

        PROGRAM TEMP_dgesv
            USE Declaration
            IMPLICIT NONE
            
            !--- message ---!
            PRINT *, "*********************************"
            PRINT *, "*** Use dgesv to solve Ax = B ***"
            PRINT *, "*********************************"
            PRINT *,
 
 
            !--- open files that store data ---!
            CALL OPEN_FILES


            !--- allocate storage ---!
            CALL DIMNS_ASSIGNMENT

            
            !--- read data ---!
            CALL GET_DATA

            
            !--- call lapack subroutine dgesv ---!
            CALL dgesv ( N, NRHS, A, LDA, IPIV, B, LDB, INFO )

            
            !--- write the solution ---! 
            CALL PRINT_SOLUTION
             
            !--- write INFO ---!
            WRITE(*, *) "INFO = ", INFO
            
            DEALLOCATE(A,B,IPIV)
        END PROGRAM TEMP_dgesv
        


        
        SUBROUTINE OPEN_FILES
            !--- obtain the dada of the matrix A ---!
            OPEN (unit=11, file='/path/to/file/of/data_A', status='old')
            
            !--- obtain the data of right hand side B ---!
            OPEN (unit=22, file='/path/to/file/of/data_B', status='old')
        END SUBROUTINE OPEN_FILES
        

	SUBROUTINE DIMNS_ASSIGNMENT
	    USE Declaration
	    !--- obtain inputs ---!
	    WRITE(*, '(A)', ADVANCE = 'NO') "Input number of columns in B. NRHS = "
            READ *, NRHS
	    WRITE(*, '(A)', ADVANCE = 'NO') "Input number of columns in matrix A. N >= 0. N = "
	    READ *, N 

	    LDA = max(1,N)
	    LDB = max(1,N)

	    !--- allocate arrays ---!
	    ALLOCATE(A(LDA, N))
	    ALLOCATE(B(LDB, NRHS))
	    ALLOCATE(IPIV(N))
	END SUBROUTINE DIMNS_ASSIGNMENT


	SUBROUTINE GET_DATA
	    USE Declaration
	    !--- read data from files ---!
	    READ(11, *) ((A(I,J),J=1,N),I=1,N)

	    !--- read data from file for B ---!
	    READ(22, *) ((B(I,J),J=1,NRHS),I=1,LDB)
	END SUBROUTINE GET_DATA


        SUBROUTINE PRINT_SOLUTION
            USE Declaration

            !--- Note: the solution is returned in B and A has been changed ---! 
            WRITE(*,*)
            WRITE(*,*) 'SOLUTION: '
            DO I = 1, LDB
                WRITE(*,11100) ( B( I, J ), J = 1, NRHS )
            END DO
            WRITE(*,*)
                   
            !--- real ---!
11100       FORMAT(11(:,1X, F8.4))            

            !--- complex ---!    
22200       FORMAT( 11(:,1X,'(',F12.6,',',F12.6,')') )
        END SUBROUTINE PRINT_SOLUTION
