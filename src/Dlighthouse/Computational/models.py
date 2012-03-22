from django.db import models
from Driver.models import RoutineInfo

# Create your models here.

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
	)

STORAGE_CHOICES = (
	(u'full', u'full'),
	(u'band', u'band'),
	(u'packed', u'packed'),
	(u'tridiagonal', u'tridiagonal'),
)  	





class LinearEquation_computational(models.Model):
	thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
	routineName = models.CharField('Routine Name', max_length=30)
	matrixType = models.CharField('Matrix Type', max_length=20, choices=MATRIX_CHOICES)
	storageType = models.CharField('Storage', max_length=20, choices=STORAGE_CHOICES)
	url = models.URLField()
	notes = models.CharField('Notes', max_length=225)
	info = models.ForeignKey(RoutineInfo)

	def __unicode__(self):
		return self.matrixType
		return self.storageType




class LinearEquation_factor(models.Model):
	thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
	routineName = models.CharField('Routine Name', max_length=30)
	matrixType = models.CharField('Matrix Type', max_length=20, choices=MATRIX_CHOICES)
	storageType = models.CharField('Storage', max_length=20, choices=STORAGE_CHOICES)
	url = models.URLField()
	notes = models.CharField('Notes', max_length=225)
	info = models.ForeignKey(RoutineInfo)

	def __unicode__(self):
		return self.matrixType
		return self.storageType




class LinearEquation_solve(models.Model):
	thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
	routineName = models.CharField('Routine Name', max_length=30)
	matrixType = models.CharField('Matrix Type', max_length=20, choices=MATRIX_CHOICES)
	storageType = models.CharField('Storage', max_length=20, choices=STORAGE_CHOICES)
	url = models.URLField()
	notes = models.CharField('Notes', max_length=225)
	info = models.ForeignKey(RoutineInfo)

	def __unicode__(self):
		return self.matrixType
		return self.storageType



class LinearEquation_condition_number(models.Model):
	thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
	routineName = models.CharField('Routine Name', max_length=30)
	matrixType = models.CharField('Matrix Type', max_length=20, choices=MATRIX_CHOICES)
	storageType = models.CharField('Storage', max_length=20, choices=STORAGE_CHOICES)
	url = models.URLField()
	notes = models.CharField('Notes', max_length=225)
	info = models.ForeignKey(RoutineInfo)

	def __unicode__(self):
		return self.matrixType
		return self.storageType



class LinearEquation_error_bound(models.Model):
	thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
	routineName = models.CharField('Routine Name', max_length=30)
	matrixType = models.CharField('Matrix Type', max_length=20, choices=MATRIX_CHOICES)
	storageType = models.CharField('Storage', max_length=20, choices=STORAGE_CHOICES)
	url = models.URLField()
	notes = models.CharField('Notes', max_length=225)
	info = models.ForeignKey(RoutineInfo)

	def __unicode__(self):
		return self.matrixType
		return self.storageType



class LinearEquation_invert(models.Model):
	thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
	routineName = models.CharField('Routine Name', max_length=30)
	matrixType = models.CharField('Matrix Type', max_length=20, choices=MATRIX_CHOICES)
	storageType = models.CharField('Storage', max_length=20, choices=STORAGE_CHOICES)
	url = models.URLField()
	notes = models.CharField('Notes', max_length=225)
	info = models.ForeignKey(RoutineInfo)

	def __unicode__(self):
		return self.matrixType
		return self.storageType




class LinearEquation_equilibrate(models.Model):
	thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
	routineName = models.CharField('Routine Name', max_length=30)
	matrixType = models.CharField('Matrix Type', max_length=20, choices=MATRIX_CHOICES)
	storageType = models.CharField('Storage', max_length=20, choices=STORAGE_CHOICES)
	url = models.URLField()
	notes = models.CharField('Notes', max_length=225)
	info = models.ForeignKey(RoutineInfo)

	def __unicode__(self):
		return self.matrixType
		return self.storageType


'''
class Eigensolver_Comput(models.Model):
	thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
	routineName = models.CharField('Routine Name', max_length=30)
	matrixType = models.CharField('Matrix Type', max_length=20, choices=MATRIX_CHOICES)
	storageType = models.CharField('Storage', max_length=20, choices=STORAGE_CHOICES)
	url = models.URLField()
	notes = models.CharField('Notes', max_length=225)
	info = models.ForeignKey(RoutineInfo)

	def __unicode__(self):
		return self.matrixType
		return self.storageType
'''


