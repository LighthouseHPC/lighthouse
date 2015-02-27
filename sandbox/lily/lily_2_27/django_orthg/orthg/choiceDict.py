#####----------- for all LAPACK routines -------------#####
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
        )


STORAGE_CHOICES = (
        (u'full',                       	u'full'),
        (u'band',                       	u'band'),
        (u'packed',                     	u'packed'),
        (u'tridiagonal',                	u'tridiagonal'),
        (u'bidiagonal',                 	u'bidiagonal'),
        (u'RFP',                        	u'RFP'),
        (u'full/packed/band/tridiagonal', 	u'full/packed/band/tridiagonal'),
        (u'bidiagonal/band',			u'bidiagonal/band'),
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

#####----------- for LAPACK least square problem type, Sylvester routines -------------#####
STANDARD_CHOICES = (
        (u'standard',                   	u'standard'),
        (u'generalized',                	u'generalized'), 
)


QR_CHOICES = (
        (u'QR Column pivoting',                   	u'QR Column pivoting'),
        (u'Faster QR',                	                u'Faster QR'), 
)

SVD_CHOICES = (
	(u'SVD',         			        u'SVD'),
        (u'divide and conquer',        			u'divide and conquer'),
)

SFULLRANK_CHOICES = (
        (u'Full Rank',                   	u'Full Rank'),
        (u'Not Full Rank',                 	u'Not Full Rank'), 
)


GFULLRANK_CHOICES = (
        (u'Neither are Full Rank',                   	u'Neither are Full Rank'),
        (u'Both are Full Rank',                 	u'Both are Full Rank'), 
        (u'Only A is Full Rank',                 	u'Only A is Full Rank'), 
        (u'Only B is Full Rank',                 	u'Only B is Full Rank'), 
)

