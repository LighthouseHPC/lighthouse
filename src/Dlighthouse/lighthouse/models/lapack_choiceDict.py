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
        (u's',                          	u'single'), 
        (u'd',                          	u'double'), 
        (u'c',                          	u'complex'), 
        (u'z',                          	u'complex double'),
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
EIGENPROBLEM_CHOICES = (
	(u'eigen',				u'Solve an eigenproblem (e.g. diagonalize a matrix to find eigenvalues)'),
	(u'Hessenberg',				u'Reduce a matrix to upper Hessenberg form'),
	(u'generalized_to_standard',		u'Reduce a generalized eigenproblem to standard form'),
	(u'cndNumber_of_evtrs',			u'Estimate condition numbers of eigenvectors'),
	(u'balance',				mark_safe(u'Balance a <em>general</em> matrix to improve eigenvalue accuracy')),
)




#####----------- for LAPACK SVD routines -------------#####
SVD_CHOICES = (
	(u'svd_standard',			mark_safe('Computing the SVD of a matrix')),
	(u'svd_generalized',			mark_safe('Computing the generalized SVD of a matrix in <em>full</em> storage')),
	(u'bidiagonal',				mark_safe('Reducing a <em>general</em> matrix to bidiagonal form')),
)


FUNCTION_STANDARD_CHOICES = (
	(u'reduceBid',				u'reduce a general matrix to bidiagonal form'),
	(u'generateQ',				mark_safe('generate <i>Q</i>')),
	(u'multiplyByQ',			mark_safe('multiplies an arbitrary matrix by <i>Q</i>')),
	(u'svdBid',				u'svd of a bidiagonal matrix'),
)


FUNCTION_GENERALIZED_CHOICES = (
	(u'reduceUpTr',				mark_safe('simultaneously reduce matrices <i>A</i> and <i>B</i> to upper triangular form')),
	(u'svdUpTr',				mark_safe('svd of upper triangular matrices <i>A</i> and <i>B</i>')),
)


FUNCTION_CHOICES = ((u'svd',	u'svd'),)+FUNCTION_STANDARD_CHOICES + FUNCTION_GENERALIZED_CHOICES

METHOD_CHOICES = (
	(u'QR',					u'QR algorithm'),
	(u'divide-and-conquer',			u'divide and conquer'),
	(u'jacobi',				u'jacobi'),
)




#####----------- for LAPACK Sylvester routines -------------#####
SYLVESTER_CHOICES = (
	(u'standard',				mark_safe('Standard ( <i>op(A)*X &#177 X*op(B) = C</i> )')),
	(u'generalized',			mark_safe('Generalized ( <i>A*R &#8722 L*B = &#945C</i> and <i>D*R &#8722 L*E = &#945F</i> )')),
)

