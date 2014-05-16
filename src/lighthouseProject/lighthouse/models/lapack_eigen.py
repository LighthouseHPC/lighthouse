from django.db import models
from lighthouse.models.lapack_choiceDict import *
from lighthouse.models.lapack_routineInfo import lapack_RoutineInfo



###---------------- for guided search ----------------###
###--- Eigenproblem ---###
class lapack_eigen(models.Model):
        thePrecision = models.CharField('Precision', max_length=10, choices=PRECISION_CHOICES)
        routineName = models.CharField('Routine Name', max_length=30)
        complexNumber = models.CharField('Complex Number', max_length=10, choices=NOYES_CHOICES)
        problem = models.CharField('Problem Type', max_length=50, choices=EIGENPROBLEM_CHOICES)
        standardGeneralized = models.CharField('Standard/Generalized', max_length=20, choices=STANDARD_CHOICES)
        matrixType = models.CharField('Matrix Type', max_length=30, choices=MATRIX_CHOICES)
        storageType = models.CharField('Storage', max_length=60, choices=STORAGE_CHOICES)
        selectedEV = models.CharField('Selected Eigenvalues', max_length=10, choices=NOYESNONE_CHOICES)
        eigenvector = models.CharField('Eigenvectors', max_length=10, choices=NOYESBOTH_CHOICES)
        schur = models.CharField('Schur form/vectors', max_length=30, choices=NOYESNONE_CHOICES)
        cndNumber = models.CharField('cndNumber/balance', max_length=10, choices=NOYESNONE_CHOICES)
        notes = models.CharField('Notes', max_length=225)
        info = models.ForeignKey(lapack_RoutineInfo)

        class Admin:
                list_display = ('id', 'thePrecision', 'routineName', 'standardGeneralized', 'matrixType', 'storageType', 'info')

        def __unicode__(self):
                return self.matrixType
                return self.storageType
        
        class Meta:
                app_label = 'lighthouse'
                
                
                