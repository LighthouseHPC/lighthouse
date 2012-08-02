from django.db import models


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
	)

STORAGE_CHOICES = (
	(u'full', u'full'),
	(u'band', u'band'),
	(u'packed', u'packed'),
	(u'tridiagonal', u'tridiagonal'),
)  	




class RoutineInfo(models.Model):
	routine = models.CharField('Routine', max_length=30)
	info = models.TextField('Information', blank=True, null=True)

	def __unicode__(self):
		return self.info





class LinearEquation_driver(models.Model):
	thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
	routineName = models.CharField('Routine Name', max_length=30)
	matrixType = models.CharField('Matrix Type', max_length=20, choices=MATRIX_CHOICES)
	storageType = models.CharField('Storage', max_length=20, choices=STORAGE_CHOICES)
	url = models.URLField()
	notes = models.CharField('Notes', max_length=225)
	info = models.ForeignKey(RoutineInfo)

        class Admin:
		list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'info')

	def __unicode__(self):
		return self.matrixType
		return self.storageType




class LinearEquation_simple(models.Model):
	thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
	routineName = models.CharField('Routine Name', max_length=30)
	matrixType = models.CharField('Matrix Type', max_length=20, choices=MATRIX_CHOICES)
	storageType = models.CharField('Storage', max_length=20, choices=STORAGE_CHOICES)
	url = models.URLField()
	notes = models.CharField('Notes', max_length=225)
	info = models.ForeignKey(RoutineInfo)

        class Admin:
		list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'info')

	def __unicode__(self):
		return self.matrixType
		return self.storageType





class LinearEquation_expert(models.Model):
	thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
	routineName = models.CharField('Routine Name', max_length=30)
	matrixType = models.CharField('Matrix Type', max_length=20, choices=MATRIX_CHOICES)
	storageType = models.CharField('Storage', max_length=20, choices=STORAGE_CHOICES)
	url = models.URLField()
	notes = models.CharField('Notes', max_length=225)
	info = models.ForeignKey(RoutineInfo)

        class Admin:
		list_display = ('id', 'thePrecision', 'routineName', 'matrixType', 'storageType', 'info')

	def __unicode__(self):
		return self.matrixType
		return self.storageType







"""
class LinearLeastSquare(models.Model):
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


class SymmetricEigenvalue(models.Model):
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


class nonSymmetricEigenvalue(models.Model):
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



class Eigensolver(models.Model):
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

"""

