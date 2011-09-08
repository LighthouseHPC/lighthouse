from django.db import models
from Drivers.models import Problem, RoutineInfo

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
	(u'orthogonal', u'orthogonal'),
	(u'unitary', u'unitary'),
	(u'diagonal', u'diagonal'),
	(u'upper Hessenberg',  u'upper Hessenberg'),
	(u'triangular', u'triangular'),
	)

STORAGE_CHOICES = (
	(u'full', u'full'),
	(u'banded', u'banded'),
	(u'packed', u'packed'),
	(u'tridiagonal', u'tridiagonal'),
)  	




class LinearEquation_comb(models.Model):
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




class LinearEquation_trans(models.Model):
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



