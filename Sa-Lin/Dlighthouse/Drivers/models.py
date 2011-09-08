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
	(u'banded', u'banded'),
	(u'packed', u'packed'),
	(u'tridiagonal', u'tridiagonal'),
)  	



class Problem(models.Model):
	problem = models.CharField('Problem', max_length = 200)
	
	def __unicode__(self):
		return self.problem




class RoutineInfo(models.Model):
	routine = models.CharField('Routine', max_length=30)
	info = models.TextField('Information', blank=True, null=True)

	def __unicode__(self):
		return self.info


class LinearEquation(models.Model):
	thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
	routineName = models.CharField('Routine Name', max_length=30)
	matrixType = models.CharField('Matrix Type', max_length=20, choices=MATRIX_CHOICES)
	structureType = models.CharField('Storage', max_length=20, choices=STORAGE_CHOICES)
	url = models.URLField()
	problem = models.ForeignKey(Problem)
	description = models.CharField('Description', max_length=225)
	info = models.ForeignKey(RoutineInfo)

	def __unicode__(self):
		return self.matrixType
		return self.structureType





class LinearLeastSquare(models.Model):
	thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
	routineName = models.CharField('Routine Name', max_length=30)
	matrixType = models.CharField('Matrix Type', max_length=20, choices=MATRIX_CHOICES)
	structureType = models.CharField('Storage', max_length=20, choices=STORAGE_CHOICES)
	url = models.URLField()
	problem = models.ForeignKey(Problem)
	description = models.CharField('Description', max_length=225)
	info = models.ForeignKey(RoutineInfo)

	def __unicode__(self):
		return self.matrixType
		return self.structureType

"""
class SymmetricEigenvalue(models.Model):
	thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
	routineName = models.CharField('Routine Name', max_length=30)
	matrixType = models.CharField('Matrix Type', max_length=20, choices=MATRIX_CHOICES)
	structureType = models.CharField('Storage', max_length=20, choices=STORAGE_CHOICES)
	url = models.URLField()
	problem = models.ForeignKey(Problem)
	description = models.CharField('Description', max_length=225)
	info = models.ForeignKey(RoutineInfo)

	def __unicode__(self):
		return self.matrixType
		return self.structureType


class nonSymmetricEigenvalue(models.Model):
	thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
	routineName = models.CharField('Routine Name', max_length=30)
	matrixType = models.CharField('Matrix Type', max_length=20, choices=MATRIX_CHOICES)
	structureType = models.CharField('Storage', max_length=20, choices=STORAGE_CHOICES)
	url = models.URLField()
	problem = models.ForeignKey(Problem)
	description = models.CharField('Description', max_length=225)
	info = models.ForeignKey(RoutineInfo)

	def __unicode__(self):
		return self.matrixType
		return self.structureType
"""


class Eigensolver(models.Model):
	thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
	routineName = models.CharField('Routine Name', max_length=30)
	matrixType = models.CharField('Matrix Type', max_length=20, choices=MATRIX_CHOICES)
	structureType = models.CharField('Storage', max_length=20, choices=STORAGE_CHOICES)
	url = models.URLField()
	problem = models.ForeignKey(Problem)
	description = models.CharField('Description', max_length=225)
	info = models.ForeignKey(RoutineInfo)

	def __unicode__(self):
		return self.matrixType
		return self.structureType



