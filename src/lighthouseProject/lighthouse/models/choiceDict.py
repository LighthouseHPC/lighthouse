#####----------- for all LAPACK routines -------------#####
MATRIX_CHOICES = (
        (u'general',                    u'general'), 
        (u'symmetric',                  u'symmetric'), 
        (u'Hermitian',                  u'Hermitian'), 
        (u'SPD',                        u'SPD'),
        (u'HPD',                        u'HPD'),
        (u'triangular',                 u'triangular'),
        (u'upper triangular',           u'upper triangular'),
        (u'SPsD',                       u'SPsD'),
        (u'HPsD',                       u'HPsD'),
        (u'upper Hessenberg',           u'upper Hessenberg'),
        (u'block upper triangular',     u'block upper triangular'),
        )


STORAGE_CHOICES = (
        (u'full',                       u'full'),
        (u'band',                       u'band'),
        (u'packed',                     u'packed'),
        (u'tridiagonal',                u'tridiagonal'),
        (u'bidiagonal',                 u'bidiagonal'),
        (u'RFP',                        u'RFP'),
        (u'full/packed/band/tridiagonal', u'full/packed/band/tridiagonal'),
)


PRECISION_CHOICES = (
        (u's',                          u'single'), 
        (u'd',                          u'double'), 
        (u'c',                          u'complex'), 
        (u'z',                          u'complex double'),
)


SINGLEDOUBLE_CHOICES = (
        (u'single',                          u'single'), 
        (u'double',                          u'double'),        
)


NOYES_CHOICES = (
        (u'no',         u'no'),
        (u'yes',        u'yes'),    
)


NOYESNONE_CHOICES = (
        (u'no',         u'no'),
        (u'yes',        u'yes'),
        (u'none',       u'none'), 
)


NOYESBOTH_CHOICES = (
        (u'no',         u'no'),
        (u'yes',        u'yes'),
        (u'no/yes',     u'no/yes'), 
)






#####----------- for LAPACK eigen, svd routines -------------#####
STANDARD_CHOICES = (
        (u'standard',                   u'standard'),
        (u'generalized',                u'generalized'), 
)





#####----------- for LAPACK eigen routines -------------#####
EIGENPROBLEM_CHOICES = (
	(u'eigen',				u'Solve an eigenproblem'),
	(u'Hessenberg',				u'Reduce a matrix to upper Hessenberg form'),
	(u'cndNumber_of_evtrs',			u'Estimate condition numbers of eigenvectors'),
	(u'balance',				u'Balance a general matrix to improve eigenvalue accuracy'),
)




#####----------- for LAPACK SVD routines -------------#####
EIGENPROBLEM_CHOICES = (
	(u'svd',				u'Computes singular value decomposition (SVD)'),
	(u'bidiagonal',				u'Reduce a matrix to bidiagonal form'),
)