        MODULE Declaration
            !--- declare arrays to be allocatable ---!
            INTEGER                                                     :: N, NRHS, LDA, LDB, INFO, I, J
            INTEGER, DIMENSION(:), ALLOCATABLE                          :: IPIV
            REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE                :: A, B
        END MODULE Declaration
                

        PROGRAM TEMP_dgesv
            USE Declaration
            IMPLICIT NONE
            
            !--- solve Ax = B ---!
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
            CALL DGESV( N, NRHS, A, LDA, IPIV, B, LDB, INFO )


            
            !--- write the solution ---!
            !--- Note: the solution is returned in B and A has been changed ---!  
            CALL PRINT_SOLUTION( B, N, NRHS)
            CALL PRINT_INFO( INFO)
            
            DEALLOCATE(A,IPIV,B)
        END PROGRAM TEMP_dgesv
        


        
        SUBROUTINE OPEN_FILES
            !--- obtain the dada of the square matrix A ---!
            OPEN (unit=11, file='data_dgesv.txt', status='old')
            
            !--- obtain the data of right hand side B ---!
            OPEN (unit=99, file='array_dgesv.txt', status='old')
        END SUBROUTINE OPEN_FILES
        

	SUBROUTINE DIMNS_ASSIGNMENT
	    USE Declaration
	    !--- obtain inputs ---!
	    WRITE(*, '(A)', ADVANCE = 'NO') 'Input leading dimension of B, LDB = '
	    READ *, LDB 
	    WRITE(*, '(A)', ADVANCE = 'NO') 'Input leading dimension of A, LDA = '
	    READ *, LDA 
	    WRITE(*, '(A)', ADVANCE = 'NO') 'Input the order of your square matrix. N = '
	    READ *, N 
	    WRITE(*, '(A)', ADVANCE = 'NO') 'Input number of columns in B. NRHS = '
	    READ *, NRHS 

	    !--- allocate matrix/array ---!
	    ALLOCATE(A(LDA,N)) 
	    ALLOCATE(IPIV(N)) 
	    ALLOCATE(B(LDB,NRHS)) 
	END SUBROUTINE DIMNS_ASSIGNMENT


	SUBROUTINE GET_DATA
	    USE Declaration
            !--- read data from file for A ---!
            READ(11, *) ((A(I,J),J=1,N),I=1,LDA)
            !--- read data from file for B ---!
            READ(99,*) ((B(I,J),J=1,NRHS),I=1,LDB)
	END SUBROUTINE GET_DATA


        SUBROUTINE PRINT_SOLUTION( B, N, NRHS )
            INTEGER             :: N, NRHS, I, J
            REAL(KIND=8)     :: B (N, NRHS)

            WRITE(*,*)
            WRITE(*,*) 'SOLUTION: '
            DO I = 1, N
                WRITE(*,11100) ( B( I, J ), J = 1, NRHS )
            END DO
            WRITE(*,*)
                   
            !--- real ---!
11100       FORMAT(11(:,1X, F6.2))            

            !--- complex ---!    
22200       FORMAT( 11(:,1X,'(',F12.6,',',F12.6,')') )
        END SUBROUTINE PRINT_SOLUTION



        SUBROUTINE PRINT_INFO( INFO)
             INTEGER             :: INFO
             
             WRITE(*, *) "info = ", INFO
        END SUBROUTINE PRINT_INFO
        
        