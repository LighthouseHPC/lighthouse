import os
from django.db import models
from django.db.models import Q


TYPE_CHOICES = (
	(u'1',	'Hermitian'),
	(u'3',	'Non-Hermitian'),
)
SLEPC_PROBTYPE =(
	('eps_hermitian'),
	('eps_non_hermitian'),
)
REAL_CHOICES = (
	('0',	'Real'),
	('1',	'Complex'),
)

SLEPC_ROUTINES = (
	( u'p','power'),
	( u's','subspace'),
	( u'a','arnoldi'),
	( u'l','lanczos'),
	( u'k','krylovschur'),
	( u'g','gd'),
	( u'j','jd'),
	( u'n','noconvergence'), 
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

SLEPC_SPECTRUMS =(
	('eps_largest_magnitude'),
	('eps_smallest_magnitude'),
	('eps_largest_real'),
	('eps_smallest_real'),
	('eps_largest_imaginary'),
	('eps_smallest_imaginary'),
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
#1. Left side of the tree
#2. Right side of the tree

class slepc_treeLeft(models.Model):
	thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
	isComplex = models.CharField('Complex', max_length=2, choices=EIGEN_YESNO_CHOICES)
	sizeLL = models.DecimalField('Matrix size LL', max_digits=12, decimal_places=2, default=0)
	sizeUL = models.DecimalField('Matrix size UL', max_digits=12, decimal_places=2, default=0)
	numProcessorsLL = models.DecimalField('Number of processors LL',max_digits=12, decimal_places=2, default=1)
	numProcessorsUL = models.DecimalField('Number of processors UL',max_digits=12, decimal_places=2, default=1)
	spectrumType = models.CommaSeparatedIntegerField('Portion of spectrum', max_length=20)
	probType = models.CommaSeparatedIntegerField('Problem type', max_length=20)
	nEigenValuesLL = models.DecimalField('Number of eigenvalues LL', max_digits=12, decimal_places=2, default=1)
	nEigenValuesUL = models.DecimalField('Number of eigenvalues UL', max_digits=12, decimal_places=2, default=1)
	toleranceLL = models.FloatField('Tolerance LL' , default=0.0001)
	toleranceUL = models.FloatField('Tolerance UL' , default=0.0001)
	isBinary = models.CharField('Binary', max_length=2, choices=EIGEN_YESNO_CHOICES)
	perfIndex = models.CommaSeparatedIntegerField('Performance index', max_length=20)
	routineName = models.CharField('Routine Name', max_length=30)

	#url = models.URLField()
	notes = models.CharField('Notes', max_length=225)
	info = models.ForeignKey(slepc_RoutineInfo)

        class Admin:
		list_display = ('id', 'thePrecision', 'isComplex','sizeLL','sizeUL','numProcessorsLL','numProcessorsUL' ,'spectrumType' , 'probType', 'nEigenValuesLL', 'nEigenValuesUL' , 'toleranceLL','toleranceUL', 'isBinary', 'perfIndex', 'routineName', 'info')

	def __unicode__(self):
		return unicode(self.spectrumType)
		return unicode(self.routineName)
	
	class Meta:
		app_label = 'lighthouse'

class slepc_treeRight(models.Model):
	thePrecision = models.CharField('Precision', max_length=20, choices=PRECISION_CHOICES)
	isComplex = models.CharField('Complex', max_length=2, choices=EIGEN_YESNO_CHOICES)
	sizeLL = models.DecimalField('Matrix size LL', max_digits=12, decimal_places=2, default=0)
	sizeUL = models.DecimalField('Matrix size UL', max_digits=12, decimal_places=2, default=0)
	numProcessorsLL = models.DecimalField('Number of processors LL',max_digits=12, decimal_places=2, default=1)
	numProcessorsUL = models.DecimalField('Number of processors UL',max_digits=12, decimal_places=2, default=1)
	spectrumType = models.CommaSeparatedIntegerField('Portion of spectrum', max_length=20)
	probType = models.CommaSeparatedIntegerField('Problem type', max_length=20)
	nEigenValuesLL = models.DecimalField('Number of eigenvalues LL', max_digits=12, decimal_places=2, default=1)
	nEigenValuesUL = models.DecimalField('Number of eigenvalues UL', max_digits=12, decimal_places=2, default=1)
	toleranceLL = models.FloatField('Tolerance LL' , default=0.0001)
	toleranceUL = models.FloatField('Tolerance UL' , default=0.0001)
	isBinary = models.CharField('Binary', max_length=2, choices=EIGEN_YESNO_CHOICES)
	perfIndex = models.CommaSeparatedIntegerField('Performance index', max_length=20)
	routineName = models.CharField('Routine Name', max_length=30)

	#url = models.URLField()
	notes = models.CharField('Notes', max_length=225)
	info = models.ForeignKey(slepc_RoutineInfo)

        class Admin:
		list_display = ('id', 'thePrecision', 'isComplex','sizeLL','sizeUL','numProcessorsLL','numProcessorsUL' ,'spectrumType' , 'probType', 'nEigenValuesLL', 'nEigenValuesUL' , 'toleranceLL','toleranceUL', 'isBinary', 'perfIndex', 'routineName', 'info')


	def __unicode__(self):
		return unicode(self.spectrumType)
		return unicode(self.routineName)
	
	class Meta:
		app_label = 'lighthouse'


def BestRoutines(data):
	#if Spectrum in {2 4} then node 2 elseif Spectrum in {1 3 5 6} then node 3 else krylovschur

	spectrumData = data['spectrum']
	
	if data['spectrum'] == SPECTRUM_CHOICES[1][0] or data['spectrum'] == SPECTRUM_CHOICES[3][0]:
		Table = slepc_treeLeft
	else:
		Table = slepc_treeRight

	if data['type'] == None:
		typeData = False
	else:
		typeData = data['type']
	
	if data['binary'] == None:
		binaryData = False
	else:
		binaryData = data['binary']
	
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
.filter(Q(probType__contains = typeData) | Q(probType__isnull = typeData)) \
.filter(Q(isComplex=complexData) | Q(isComplex__isnull =complexData )) \
.filter(Q(isBinary=binaryData) | Q(isBinary__isnull =binaryData )) \
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


def getScript(data):

	filepath = './lighthouse/database/slepc_eprob/work_dir/scriptTempComplex.sh'
	if data['complex'] == EIGEN_YESNO_CHOICES[1][0]:
		filepath = './lighthouse/database/slepc_eprob/work_dir/scriptTempReal.sh'
	
	if os.path.isfile(filepath):
	   	with open(filepath, 'r') as f:
			script = f.read()
	

	scriptRun = 'mpirun -np '

	if data['processors'] != None:
		scriptRun = scriptRun + str(data['processors'])

	scriptRun = scriptRun + ' ./eigenvalue_parallel -fin <Enter input matrix file location (in binary format)>' 
	
	
	if data['spectrum'] != None:
		specNum = int(data['spectrum'][0])-1;
		scriptRun = scriptRun + ' -' + str(SLEPC_SPECTRUMS[specNum]);

	
	if data['type'] != None:
		typeNum = int(data['type'][0]) -1;
		if typeNum==2:
			typeNum=1;
		scriptRun = scriptRun + ' -' + str(SLEPC_PROBTYPE[typeNum]);
	
	if data['numEigenvalues'] != None:
		scriptRun = scriptRun + ' -eps_nev ' + str(data['numEigenvalues']) 

	if data['tolerance'] != None:
		scriptRun = scriptRun + ' -eps_tol ' + str(data['tolerance'])

	script += scriptRun
	return script
	

