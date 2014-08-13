from django.db import models
from lighthouse.models.lapack_choiceDict import *
from lighthouse.models.lapack_routineInfo import lapack_RoutineInfo



###---------------- for guided search ----------------###
###--- Eigenproblem ---###
class lapack_eigen_guided(models.Model):
        thePrecision = models.CharField('precision', max_length=10, choices=PRECISION_CHOICES)
        routineName = models.CharField('routine name', max_length=30)
        problem = models.CharField('problem type', max_length=50, choices=EIGENPROBLEM_CHOICES)
        standardGeneralized = models.CharField('standard/generalized', max_length=20, choices=STANDARD_CHOICES)
        complexNumber = models.CharField('complex number', max_length=10, choices=NOYES_CHOICES)
        matrixType = models.CharField('matrix type', max_length=30, choices=MATRIX_CHOICES)
        storageType = models.CharField('storage type', max_length=60, choices=STORAGE_CHOICES)
        selectedEV = models.CharField('selected eigenvalues', max_length=10, choices=NOYESNONE_CHOICES)
        eigenvector = models.CharField('eigenvectors', max_length=10, choices=NOYESBOTH_CHOICES)
        schur = models.CharField('Schur form/vectors', max_length=30, choices=NOYESNONE_CHOICES)
        cndNumber = models.CharField('cndNumber/balance', max_length=10, choices=NOYESNONE_CHOICES)
        singleDouble = models.CharField('single/double', max_length=10, choices=SINGLEDOUBLE_CHOICES)
        notes = models.CharField('notes', max_length=225)
        info = models.ForeignKey(lapack_RoutineInfo)

        class Admin:
                list_display = ('id', 'thePrecision', 'routineName', 'standardGeneralized', 'matrixType', 'storageType', 'info')

        def __unicode__(self):
                return self.matrixType
                return self.storageType
        
        class Meta:
                app_label = 'lighthouse'
                
                
                