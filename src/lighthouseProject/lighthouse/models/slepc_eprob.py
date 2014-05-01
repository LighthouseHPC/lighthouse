from django.db import models


SLEPC_ROUTINES = (
	( u'power'),
	( u'subspace'),
	( u'arnoldi'),
	( u'lanczos'),
	( u'krylovschur'),
	( u'gd'),
	( u'jd'),
	( u'noconvergence'), 
)
PRECISION_CHOICES = (
	(u's', u's(single)'), 
	(u'd', u'd(double)'), 
)

SPECTRUM_CHOICES = (
	(u'lm', u'Largest magnitude'), 
	(u'sm', u'Smallest magnitude'), 
	(u'lr', u'Largest real'), 
	(u'sr', u'Smallest real'), 
	(u'li', u'Largest imaginary'), 
	(u'si', u'Smallest imaginary'), 
#ADD target
)

EIGEN_YESNO_CHOICES = (
    (u'y'    ,u'Yes'),
    (u'n'    ,u'No')
)

""" for routine information  """
class slepc_RoutineInfo(models.Model):
	routine = models.CharField('Routine', max_length=30)
	info = models.TextField('Information', blank=True, null=True)

	def __unicode__(self):
		return self.info
	class Meta:
		app_label = 'lighthouse'		



# Create your models here.
# table with respect to matrix type
# two tables with exactly the same structure
#1. Hermitian
#2. Non Hermitian

class slepc_Hermitian(models.Model):
	thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
	isComplex = models.CharField('Complex', max_length=1, choices=EIGEN_YESNO_CHOICES)
        sizeLL = models.DecimalField('Matrix size LL', max_digits=12, decimal_places=2, default=0)
	sizeUL = models.DecimalField('Matrix size UL', max_digits=12, decimal_places=2, default=0)
	numProcessorsLL = models.DecimalField('Number of processors LL',max_digits=12, decimal_places=2, default=1)
	numProcessorsUL = models.DecimalField('Number of processors UL',max_digits=12, decimal_places=2, default=1)
	spectrumType = models.CommaSeparatedIntegerField('Portion of spectrum', max_length=20)
	nEigenValuesLL = models.DecimalField('Number of eigenvalues LL', max_digits=12, decimal_places=2, default=1)
	nEigenValuesUL = models.DecimalField('Number of eigenvalues UL', max_digits=12, decimal_places=2, default=1)
	toleranceLL = models.FloatField('Tolerance LL' , default=0.0001)
	toleranceUL = models.FloatField('Tolerance UL' , default=0.0001)
	routineName = models.CharField('Routine Name', max_length=30)

	url = models.URLField()
	notes = models.CharField('Notes', max_length=225)
	info = models.ForeignKey(slepc_RoutineInfo)

        class Admin:
		list_display = ('id', 'thePrecision', 'isComplex','sizeLL','sizeUL','numProcessorsLL','numProcessorsUL' ,'spectrumType' , 'nEigenValuesLL', 'nEigenValuesUL' , 'toleranceLL','toleranceUL', 'routineName', 'info')

	def __unicode__(self):
		return self.spectrumType
		return self.routineName
	
	class Meta:
		app_label = 'lighthouse'

class slepc_NonHermitian(models.Model):
	thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
	isComplex = models.CharField('Complex', max_length=1, choices=EIGEN_YESNO_CHOICES)
        sizeLL = models.DecimalField('Matrix size LL', max_digits=12, decimal_places=2, default=0)
	sizeUL = models.DecimalField('Matrix size UL', max_digits=12, decimal_places=2, default=0)
	numProcessorsLL = models.DecimalField('Number of processors LL',max_digits=12, decimal_places=2, default=1)
	numProcessorsUL = models.DecimalField('Number of processors UL',max_digits=12, decimal_places=2, default=1)
	spectrumType = models.CommaSeparatedIntegerField('Portion of spectrum', max_length=20)
	nEigenValuesLL = models.DecimalField('Number of eigenvalues LL', max_digits=12, decimal_places=2, default=1)
	nEigenValuesUL = models.DecimalField('Number of eigenvalues UL', max_digits=12, decimal_places=2, default=1)
	toleranceLL = models.FloatField('Tolerance LL' , default=0.0001)
	toleranceUL = models.FloatField('Tolerance UL' , default=0.0001)
	routineName = models.CharField('Routine Name', max_length=30)

	url = models.URLField()
	notes = models.CharField('Notes', max_length=225)
	info = models.ForeignKey(slepc_RoutineInfo)

        class Admin:
		list_display = ('id', 'thePrecision', 'isComplex','sizeLL','sizeUL','numProcessorsLL','numProcessorsUL' ,'spectrumType' , 'nEigenValuesLL', 'nEigenValuesUL' , 'toleranceLL','toleranceUL', 'routineName', 'info')

	def __unicode__(self):
		return self.spectrumType
		return self.routineName
	
	class Meta:
		app_label = 'lighthouse'


