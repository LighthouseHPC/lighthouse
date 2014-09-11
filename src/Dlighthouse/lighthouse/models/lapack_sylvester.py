from django.db import models
from lighthouse.models.lapack_choiceDict import *
from lighthouse.models.lapack_routineInfo import lapack_RoutineInfo



###---------------- for guided search ----------------###
###--- Sylvester ---###
class lapack_sylvester(models.Model):
        thePrecision = models.CharField('Precision', max_length=10, choices=PRECISION_CHOICES)
        routineName = models.CharField('Routine Name', max_length=30)
        complexNumber = models.CharField('Complex Number', max_length=10, choices=NOYES_CHOICES)
        standardGeneralized = models.CharField('Standard/Generalized', max_length=20, choices=STANDARD_CHOICES)
        matrixType = models.CharField('Matrix Type', max_length=30, choices=MATRIX_CHOICES)
        storageType = models.CharField('Storage', max_length=60, choices=STORAGE_CHOICES)
        singleDouble = models.CharField('single/double', max_length=10, choices=SINGLEDOUBLE_CHOICES)
        info = models.ForeignKey(lapack_RoutineInfo)

        def __unicode__(self):
                return self.thePrecision+self.routineName
        
        class Meta:
                app_label = 'lighthouse'