from django.db import models
from lighthouse.models.lapack_choiceDict import *
from lighthouse.models.lapack_routineInfo import lapack_RoutineInfo



###---------------- for guided search ----------------###
class lapack_svd(models.Model):
        thePrecision = models.CharField('Precision', max_length=10, choices=PRECISION_CHOICES)
        routineName = models.CharField('Routine Name', max_length=30)
        complexNumber = models.CharField('Complex Number', max_length=10, choices=NOYES_CHOICES)
        problem = models.CharField('Problem Type', max_length=20, choices=SVD_CHOICES)
        matrixType = models.CharField('Matrix Type', max_length=30, choices=MATRIX_CHOICES)
        storageType = models.CharField('Storage', max_length=60, choices=STORAGE_CHOICES)
        singularVectors = models.CharField('singular vectors', max_length=10, choices=NOYESBOTH_CHOICES)
        singleDouble = models.CharField('single/double', max_length=10, choices=SINGLEDOUBLE_CHOICES)
        notes = models.CharField('Notes', max_length=225)
        info = models.ForeignKey(lapack_RoutineInfo)

        def __unicode__(self):
                return self.thePrecision+self.routineName
        
        class Meta:
                app_label = 'lighthouse'



###---------------- for advanced search ----------------###
class lapack_svd_advanced(models.Model):
        thePrecision = models.CharField('Precision', max_length=10, choices=PRECISION_CHOICES)
        routineName = models.CharField('Routine Name', max_length=30)
        driverComput = models.CharField('driver/computational', max_length=15, choices=DRIVERCOMPUT_CHOICES)
        standardGeneralized = models.CharField('Standard/Generalized', max_length=20, choices=STANDARD_CHOICES)
        function = models.CharField('Function', max_length=20, choices=FUNCTION_CHOICES)
        description = models.CharField('Description', max_length=100)
        method = models.CharField('Method', max_length=20, choices=METHOD_CHOICES)
        complexNumber = models.CharField('Complex Number', max_length=10, choices=NOYES_CHOICES)
        matrixType = models.CharField('Matrix Type', max_length=30, choices=MATRIX_CHOICES)
        storageType = models.CharField('Storage', max_length=60, choices=STORAGE_CHOICES)
        singularVectors = models.CharField('Singular vectors', max_length=10, choices=NOYESBOTH_CHOICES)
        singleDouble = models.CharField('Single/Double', max_length=10, choices=SINGLEDOUBLE_CHOICES)
        info = models.ForeignKey(lapack_RoutineInfo)

        def __unicode__(self):
                return self.thePrecision+self.routineName
        
        class Meta:
                app_label = 'lighthouse'