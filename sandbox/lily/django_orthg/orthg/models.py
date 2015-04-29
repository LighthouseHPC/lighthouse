import datetime
from django.utils import timezone
from datetime import datetime, date, time, timedelta
from django.db import models
from orthg.choiceDict import *
#from lighthouse.models.lapack_routineInfo import lapack_RoutineInfo

class lapack_RoutineInfo(models.Model):
	routine = models.CharField('Routine', max_length=30)
	info = models.TextField('Information', null=True, blank=True, default='')

	def __unicode__(self):
		return self.info

	class Meta:
		app_label = 'orthg'


###---------------- for guided search ----------------###
###--- Orthogonal Problem ---###
class least(models.Model):
        thePrecision = models.CharField('precision', max_length=10, choices=PRECISION_CHOICES)
        routineName = models.CharField('routine name', max_length=30)
        standardGeneralized = models.CharField('standard/generalized', max_length=20, choices=STANDARD_CHOICES)
        complexNumber = models.CharField('complex number', max_length=10, choices=NOYES_CHOICES)
        FullStorage = models.CharField('storage and full', max_length=60, choices=FullStorageNoYes_CHOICES)
        sFullRank = models.CharField('sFullRank', max_length=225,choices=NOYES_CHOICES)
        gFullRank = models.CharField('gFullRank', max_length=225,choices=GFULLRANK_CHOICES)
        singleDouble = models.CharField('single/double', max_length=10, choices=SINGLEDOUBLE_CHOICES)
        notes = models.CharField('notes', max_length=225)
        info = models.ForeignKey(lapack_RoutineInfo)

        def __unicode__(self):
                return self.thePrecision+self.routineName
                        
        class Meta:
                app_label = 'orthg'

'''class factorization(models.Model):
        thePrecision = models.CharField('precision', max_length=10, choices=PRECISION_CHOICES)
        routineName = models.CharField('routine name', max_length=30)
        standardGeneralized = models.CharField('standard/generalized', max_length=20, choices=STANDARD_CHOICES)
        complexNumber = models.CharField('complex number', max_length=10, choices=NOYES_CHOICES)
        StorageType = models.CharField('storage and full', max_length=60, choices=STORAGE_CHOICES) #full and packed
        MatrixType = models.CharField('matrix type', max_length=60, choices=MATRIX_CHOICES)
        sFullRank = models.CharField('sFullRank', max_length=225,choices=NOYES_CHOICES)
        generateQ = models.CharField('generate q', max_length=225,choices=NOYES_CHOICES)
        trapezoidal = models.CharField('trapezoidal matrix', max_length=225,choices=NOYES_CHOICES) #
        gFullRank = models.CharField('gFullRank', max_length=225,choices=GFULLRANK_CHOICES)#
        upperTrian = models.CharField('upper Triangular', max_length=225,choices=GFULLRANK_CHOICES)#
        lowerTrian = models.CharField('upper Triangular', max_length=225,choices=GFULLRANK_CHOICES)#
        singleDouble = models.CharField('single/double', max_length=10, choices=SINGLEDOUBLE_CHOICES)
        notes = models.CharField('notes', max_length=225)
        info = models.ForeignKey(lapack_RoutineInfo)

        def __unicode__(self):
                return self.thePrecision+self.routineName
                        
        class Meta:
                app_label = 'orthg'
'''


