PRECISION_CHOICES = (
        (u's',                          u'single'), 
        (u'd',                          u'double'), 
        (u'c',                          u'complex'), 
        (u'z',                          u'complex double'),
)

EIGENPROBLEM_CHOICES = (
	(u'eigen',				u'solve an eigenproblem'),
	(u'Hessenberg',				u'reduce a matrix to upper Hessenberg form'),
	(u'cndNumber_of_evtrs',			u'estimate condition numbers of eigenvectors'),
	(u'balance',				u'balance matrix to improve eigenvalue accuracy'),
)



STANDARD_CHOICES = (
        (u'standard',                   u'standard'),
        (u'generalized',                u'generalized'), 
)

MATRIX_CHOICES = (
        (u'general',                    u'general'), 
        (u'symmetric',                  u'symmetric'), 
        (u'Hermitian',                  u'Hermitian'), 
        (u'SPD',                        u'SPD'),
        (u'HPD',                        u'HPD'),
        (u'triangular',                 u'triangular'),
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
        (u'RFP',                        u'RFP'),
        (u'full/packed/band/tridiagonal', u'full/packed/band/tridiagonal'),
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