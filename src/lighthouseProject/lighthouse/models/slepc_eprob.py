from django.db import models
from django.db.models import Q


TYPE_CHOICES = (
	('Hermitian',	'Hermitian'),
	('Non-Hermitian',	'Non-Hermitian'),
)
REAL_CHOICES = (
	('0',	'Real'),
	('1',	'Complex'),
)

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
	(u's', u'Single'), 
	(u'd', u'Double'), 
)

SPECTRUM_CHOICES = (
	(u'1', u'Largest magnitude'), 
	(u'2', u'Smallest magnitude'), 
	(u'3', u'Largest real'), 
	(u'4', u'Smallest real'), 
	(u'5', u'Largest imaginary'), 
	(u'6', u'Smallest imaginary'), 
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
		return unicode(self.info)
	class Meta:
		app_label = 'lighthouse'		



# Create your models here.
# table with respect to matrix type
# two tables with exactly the same structure
#1. Hermitian
#2. Non Hermitian

class slepc_Hermitian(models.Model):
	thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
	isComplex = models.CharField('Complex', max_length=2, choices=EIGEN_YESNO_CHOICES)
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
		return unicode(self.spectrumType)
		return unicode(self.routineName)
	
	class Meta:
		app_label = 'lighthouse'

class slepc_NonHermitian(models.Model):
	thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
	isComplex = models.CharField('Complex', max_length=2, choices=EIGEN_YESNO_CHOICES)
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
		return unicode(self.spectrumType)
		return unicode(self.routineName)
	
	class Meta:
		app_label = 'lighthouse'


def BestRoutines(data):
	type=data['type']
	if type==TYPE_CHOICES[0][1]:
		Table = slepc_Hermitian
	elif type==TYPE_CHOICES[1][1]:
		Table = slepc_NonHermitian
	
	if data['spectrum'] == None:
		spectrumData = False
	else:
		spectrumData = data['spectrum']
	
	if data['complex'] == None:
		complexData = False
	else:
		complexData = data['complex']
	
	if data['size'] == None:
		sizeData = False
	else:
		sizeData = data['size']
	
	if data['numEigenvalues'] == None:
		nEVData = False
	else:
		nEVData = data['numEigenvalues']

	if data['tolerance'] == None:
		tolData = False
	else:
		tolData = data['tolerance']
	
	if data['processors'] == None:
		procData = False
	else:
		procData = data['processors']


	return Table.objects\
.filter(Q(spectrumType__contains = spectrumData) | Q(spectrumType__isnull = spectrumData)) \
.filter(Q(isComplex=complexData) | Q(isComplex__isnull =complexData )) \
.filter(Q(Q(sizeLL__lte=sizeData) & Q(sizeUL__gt=sizeData)) | Q(sizeLL__isnull=sizeData))  \
.filter(Q(Q(nEigenValuesLL__lte=nEVData) & Q(nEigenValuesUL__gt=nEVData)) | Q(nEigenValuesLL__isnull=sizeData))   \
.filter(Q(Q(toleranceLL__lte=tolData) & Q(toleranceUL__gt=tolData)) | Q(toleranceLL__isnull=sizeData)) \
.filter(Q(Q(numProcessorsLL__lte=procData) & Q(numProcessorsUL__gt=procData))  | Q(numProcessorsLL__isnull=sizeData))

	
def getSelectedRoutines(data):
	#check bad case
	#if data == []:
	#	return rows
	rows = BestRoutines(data).values_list('info', flat=True).distinct()
	db = slepc_RoutineInfo.objects.filter(id__in=rows)
	return db

