import datetime
from django.utils import timezone
from datetime import datetime, date, time, timedelta
from django.db import models
from orthg.choiceDict import *
#from lighthouse.models.lapack_routineInfo import lapack_RoutineInfo


###---------------- for guided search ----------------###
###--- Orthogonal Problem ---###
class orthg(models.Model):
        thePrecision = models.CharField('precision', max_length=10, choices=PRECISION_CHOICES)
        routineName = models.CharField('routine name', max_length=30)
        standardGeneralized = models.CharField('standard/generalized', max_length=20, choices=STANDARD_CHOICES)
        complexNumber = models.CharField('complex number', max_length=10, choices=NOYES_CHOICES)
        FullStorage = models.CharField('storage and full', max_length=60, choices=STORAGE_CHOICES)
        sFullRank = models.CharField('sFullRank', max_length=225,choices=NOYES_CHOICES)
        gFullRank = models.CharField('gFullRank', max_length=225,choices=GFULLRANK_CHOICES)
        svd = models.CharField('svd', max_length=225,choices=SVD_CHOICES)
        qr = models.CharField('qr', max_length=225,choices=QR_CHOICES)
        singleDouble = models.CharField('single/double', max_length=10, choices=SINGLEDOUBLE_CHOICES)
        notes = models.CharField('notes', max_length=225)
        #info = models.ForeignKey(lapack_RoutineInfo)

        class orthg_Admin:
                list_display = ('id', 'thePrecision', 'standardGeneralized', 'sFullRank', 'gFullRank', 'svd', 'qr', )
        def __unicode__(self):
                return self.sFullRank
                return self.gFullRank
                return self.svd
                return self.qr
                        
        class Meta:
                app_label = 'orthg'

