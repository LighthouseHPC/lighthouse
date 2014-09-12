from django.utils.safestring import mark_safe


#####----------- for all LAPACK routines -------------#####
DRIVERCOMPUT_CHOICES = (
	(u'driver',                    		u'driver'),
	(u'computational',                    	u'computational'),
)


MATRIX_CHOICES = (
        (u'general',                    	u'general'), 
        (u'symmetric',                  	u'symmetric'), 
        (u'Hermitian',                  	u'Hermitian'), 
        (u'SPD',                        	u'SPD'),
        (u'HPD',                        	u'HPD'),
        (u'triangular',                 	u'triangular'),
        (u'upper triangular',           	u'upper triangular'),
        (u'SPsD',                       	u'SPsD'),
        (u'HPsD',                       	u'HPsD'),
        (u'upper Hessenberg',           	u'upper Hessenberg'),
        (u'block upper triangular',     	u'block upper triangular'),
	(u'symmetric-definite',			u'symmetric-definite'),
	(u'Hermitian-definite',			u'Hermitian-definite'),
	(u'upper quasi-triangular',		u'upper quasi-triangular'),
	(u'bidiagonal',                 	u'bidiagonal'),
	(u'orthogonal',				u'orthogonal'),
	(u'unitary',				u'unitary'),
	(u'upper quasi-triangular',		u'upper quasi-triangular'),
	(u'diagonal',				u'diagonal'),
        )


STORAGE_CHOICES = (
        (u'full',                       	u'full'),
        (u'band',                       	u'band'),
        (u'packed',                     	u'packed'),
        (u'tridiagonal',                	u'tridiagonal'),
        (u'RFP',                        	u'RFP'),
        (u'full/packed/band/tridiagonal', 	u'full/packed/band/tridiagonal'),
)


PRECISION_CHOICES = (
        (u's',                          	u's'), 
        (u'd',                          	u'd'), 
        (u'c',                          	u'c'), 
        (u'z',                          	u'z'),
)


SINGLEDOUBLE_CHOICES = (
        (u'single',                          	u'single'), 
        (u'double',                          	u'double'),        
)


NOYES_CHOICES = (
        (u'no',         			u'no'),
        (u'yes',        			u'yes'),    
)


NOYESNONE_CHOICES = (
        (u'no',         			u'no'),
        (u'yes',        			u'yes'),
        (u'none',       			u'none'), 
)


NOYESBOTH_CHOICES = (
        (u'no',         			u'no'),
        (u'yes',        			u'yes'),
        (u'no/yes',     			u'no/yes'), 
)






#####----------- for LAPACK eigen, Sylvester routines -------------#####
STANDARD_CHOICES = (
        (u'standard',                   	u'standard'),
        (u'generalized',                	u'generalized'), 
)





#####----------- for LAPACK eigen routines -------------#####
###--- guided search ---###
EIGENPROBLEM_CHOICES = (
	(u'eigen',				u'Solve an eigenproblem (e.g. diagonalize a matrix to find eigenvalues)'),
	(u'Hessenberg',				u'Reduce a matrix to upper Hessenberg form'),
	(u'generalized_to_standard',		u'Reduce a generalized eigenproblem to standard form'),
	(u'cndNumber_of_evtrs',			u'Estimate condition numbers of eigenvectors'),
	(u'balance',				mark_safe(u'Balance a <em>general</em> matrix to improve eigenvalue accuracy')),
)


EGNVECTORMETHOD_CHOICES = (
	(u'QL/QR',				u'QL/QR'),
	(u'divide and conquer',			u'divide and conquer'),
	(u'relatively robust representations',	u'relatively robust representations'),
	(u'QL/QR or inverse iteration',		u'QL/QR or inverse iteration'),
	(u'QR',					u'QR'),
	(u'QZ and back transformation',		u'QZ and back transformation'),
)


EIGENMENU_CHOICES = (
	(u'driver_standard_sh', 		u'Driver standard with symmetric/Hermitian matrices'),
	(u'driver_standard_g', 			u'Driver standard with general matrices'),
	(u'driver_generalized_sh', 		u'Driver generalized with symmetric/Hermitian matrices'),
	(u'driver_generalized_g', 		u'Driver generalized with general matrices'),
	(u'computational_standard_sh', 		u'Computational standard with symmetric/Hermitian matrices'),
	(u'computational_standard_g', 		u'Computational standard with general matrices'),
	(u'computational_generalized_sh', 	u'Computational generalized with symmetric/Hermitian matrices'),
	(u'computational_generalized_g', 	u'Computational generalized with general matrices'),
)


###--- advanced search ---###
FUNCTION_dsg_CHOICES = (
	(u'eigenvectors',			u'compute the eigenvalues and, optionally, the left and/or right eigenvectors'),
	(u'Schur',				u'compute the eigenvalues, the Schur form, and, optionally, the matrix of Schur vectors'),
)


FUNCTION_dgg_CHOICES =(
	(u'eigenvectors',			u'compute for a pair of N-by-N nonsymmetric matrices (A,B), the generalized eigenvalues, and optionally, the left and/or right generalized eigenvectors'),
	(u'Schur',				u'compute the generalized eigenvalues, the generalized complex Schur form, and optionally left and/or right Schur vectors'),	
)


FUNCTION_cssh_CHOICES = (
	(u'reduce', 				u'reduce matrix A to real symmetric tridiagonal form'),
	(u'generateQ',				u'generate an orthogonal/unitary matrix Q'),
	(u'multiplyByQ',			u'multiply an arbitrary matrix by the matrix Q'),
	(u'eigenvalues',			u'compute eigenvalues'),
	(u'eigenvectors',			u'compute the eigenvectors of a real symmetric tridiagonal matrix T corresponding to specified eigenvalues'),
)



FUNCTION_csg_CHOICES = (
	(u'balance',				u'balance matrix A'),
	(u'reduce',				u'reduce matrix A to upper Hessenberg form H'),
	(u'generateQ',				u'generates an orthogonal/unitary matrix Q '),
	(u'multiplyByQ',			u'multiply an arbitrary matrix by the matrix Q'),
	(u'eigenvalues',			u'compute eigenvalues, Schur form, Schur vectors of H'),
	(u'eigenvectors',			u'compute eigenvectors'),
	(u'reorder',				u'reorder the Schur factorization of a general matrix'),
	(u'cndtNumber',				u'estimate the condition numbers of eigenvectors'),
)


FUNCTION_cgsh_CHOICES = (
	(u'reduce',				u'reduce a generalized eigenproblem to standard form'),
	(u'split',				u'compute a split Cholesky factorization of a symmetric/Hermitian positive definite band matrix A'),
)


FUNCTION_cgg_CHOICES = (
	(u'balance',				u'balance a pair of general matrices (A, B)'),
	(u'reduce',				u'reduce a pair of matrices (A,B) to generalized upper Hessenberg form'),
	(u'eigenvalues',			u'compute the eigenvalues of a matrix pair (H,T)'),
	(u'eigenvectors',			u'compute some or all of the right and/or left eigenvectors of a pair of matrices (S,P)'),
	(u'reorder',				u'reorder the generalized Schur decomposition of a matrix pair (A,B)'),
	(u'cndtNumber',				u'estimate reciprocal condition numbers'),
	(u'eigenvectorsG',			u'form the right or left eigenvectors of a generalized eigenvalue problem'),
)


METHOD_dssh_CHOICES = (
	(u'QL/QR',				u'QL/QR'),
	(u'divide and conquer',			u'divide and conquer'),
	(u'relatively robust representations',	u'relatively robust representations'),
	(u'QL/QR or inverse iteration',		u'QL/QR or inverse iteration'),
)


METHOD_dgsh_CHOICES = (
	(u'QL/QR',				u'QL/QR'),
	(u'divide and conquer',			u'divide and conquer'),
	(u'QL/QR or inverse iteration',		u'QL/QR or inverse iteration'),
)


METHOD_cssh_CHOICES = (
	(u'QL/QR',				u'QL/QR'),
	(u'divide and conquer',			u'divide and conquer'),
	(u'relatively robust representations',	u'relatively robust representations'),
	(u'inverse iteration',			u'inverse iteration'),
)


METHOD_csg_CHOICES = (
	(u'QL/QR',				u'QL/QR'),
	(u'inverse iteration',			u'inverse iteration'),
	(u'back transformation',		u'back transformation'),
)







#####----------- for LAPACK SVD routines -------------#####
SVD_CHOICES = (
	(u'svd_standard',			mark_safe('Computing the SVD of a matrix')),
	(u'svd_generalized',			mark_safe('Computing the generalized SVD of a matrix in <em>full</em> storage')),
	(u'bidiagonal',				mark_safe('Reducing a <em>general</em> matrix to bidiagonal form')),
)


SVDMENU_CHOICES = (
	(u'driver_standard',			u'Driver standard SVD routines'),
	(u'driver_generalized',			u'Driver generalized SVD routines'),
	(u'computational_standard',		u'Computational standard SVD routines'),
	(u'computational_generalized',		u'Computational generalized SVD routines'),
)


FUNCTION_STANDARD_CHOICES = (
	(u'reduceBid',				mark_safe('reduce a general <em>full</em> matrix to bidiagonal form')),
	(u'reduceBid_band',			mark_safe('reduce a general <em>band</em> matrix to bidiagonal form')),
	(u'generateQ',				mark_safe('generate <i>Q</i>')),
	(u'multiplyByQ',			mark_safe('multiply an arbitrary matrix by <i>Q</i>')),
	(u'svdBid',				u'compute the SVD of a bidiagonal matrix'),
)


FUNCTION_GENERALIZED_CHOICES = (
	(u'reduceUpTr',				mark_safe('simultaneously reduce general matrices <i>A</i> and <i>B</i> to upper triangular form')),
	(u'svdUpTr',				mark_safe('compute the SVD of upper triangular matrices <i>A</i> and <i>B</i>')),
)


FUNCTION_CHOICES = ((u'svd',	u'svd'),)+FUNCTION_STANDARD_CHOICES + FUNCTION_GENERALIZED_CHOICES

METHOD_CHOICES = (
	(u'QR',					u'QR algorithm'),
	(u'divide-and-conquer',			u'divide and conquer'),
	(u'jacobi',				u'Jacobi'),
)




#####----------- for LAPACK Sylvester routines -------------#####
SYLVESTER_CHOICES = (
	(u'standard',				mark_safe('Standard ( <i>op(A)*X &#177 X*op(B) = C</i> )')),
	(u'generalized',			mark_safe('Generalized ( <i>A*R &#8722 L*B = &#945C</i> and <i>D*R &#8722 L*E = &#945F</i> )')),
)

