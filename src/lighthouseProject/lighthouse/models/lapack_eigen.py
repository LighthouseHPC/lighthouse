from django.db import models
from lighthouse.models.lapack_le import lapack_RoutineInfo


PRECISION_CHOICES = (
        (u's', u's(single)'), 
        (u'd', u'd(double)'), 
        (u'c', u'c(complex)'), 
        (u'z', u'z(complex double)'),
)

MATRIX_CHOICES = (
        (u'general', u'general'), 
        (u'symmetric', u'symmetric'), 
        (u'Hermitian', u'Hermitian'), 
        (u'SPD', u'SPD'),
        (u'HPD', u'HPD'),
        (u'triangular', u'triangular'),
        (u'SPsD', u'SPsD'),
        (u'HPsD', u'HPsD'),
        )

STORAGE_CHOICES = (
        (u'full', u'full'),
        (u'band', u'band'),
        (u'packed', u'packed'),
        (u'tridiagonal', u'tridiagonal'),
        (u'RFP', u'RFP'),
)

NOYES_CHOICES = (
        (u'no', u'no'),
        (u'yes', u'yes'),    
)

NOYESNONE_CHOICES = (
        (u'no', u'no'),
        (u'yes', u'yes'),
        (u'none', u'none'), 
)


###--- for guided search -- Standard Eigenproblem ---###
class lapack_eigen_standard(models.Model):
        thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
        routineName = models.CharField('Routine Name', max_length=30)
        complexNumber = models.CharField('Complex Number', max_length=20, choices=NOYES_CHOICES)
        matrixType = models.CharField('Matrix Type', max_length=20, choices=MATRIX_CHOICES)
        storageType = models.CharField('Storage', max_length=30, choices=STORAGE_CHOICES)
        selectedEV = models.CharField('Selected Eigenvalues', max_length=20, choices=NOYESNONE_CHOICES)
        eigenvector = models.CharField('Eigenvectors', max_length=20, choices=NOYESNONE_CHOICES)
        eigenvector_schur = models.CharField('Eigenvectors/Schur', max_length=20, choices=NOYESNONE_CHOICES)
        cndNumber = models.CharField('cndNumber/blance', max_length=20, choices=NOYESNONE_CHOICES)
        notes = models.CharField('Notes', max_length=225)
        info = models.ForeignKey(lapack_RoutineInfo)

        class Admin:
                list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'info')

        def __unicode__(self):
                return self.matrixType
                return self.storageType
        
        class Meta:
                app_label = 'lighthouse'