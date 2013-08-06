from django.db import models


EPROB_PROBLEM_CHOICES = (
	(u'eval' 				,u'Eigenproblem'),
	(u'svd'					,u'Singular Value Decomposition')
)

EPROB_YESNO_CHOICES = (
	(u'y'	,			u'Yes'),
	(u'n'	,			u'No')
)

EPROB_MATRIX_CHOICES = (
	(u'gen',			u'General Matrix'),
	(u'sym',			u'Symmetric Matrix'),
	(u'her',			u'Hermitian Matrix'),	
)

EPROB_STORAGE_CHOICES = (
	(u'full',			u'Full Matrix'),
	(u'band',			u'Band Matrix'),
	(u'pack',			u'Packed Matrix'),
	(u'tri',			u'Tridiagonal Matrix'),
)

EPROB_PRECISION_CHOICES = (
	(u's',			u'Single Precision'),
	(u'd',			u'Double Precision'),
)

EPROB_ALGORITHM_CHOICES = (
	(u'n', 			u'Default algorithm'),
	(u'dc',			u'Divide and Conquer'),
	(u'rrr',		u'Relatively Robust Representation'),
	(u'mrrr',		u'Multiple Relatively Robust Representation'),
)


""" for routine information """
class lapack_eprob_RoutineInfo(models.Model):
	routine = models.CharField('Routine', max_length=30)
	info = models.TextField('Information', blank=True, null=True)

	def __unicode__(self):
		return self.info

	class Meta:
		app_label = 'lighthouse'




class lapack_eprob_simple(models.Model):
	generalized = models.CharField('Generalized', max_length=1, choices=EPROB_YESNO_CHOICES)
	kind = models.CharField('Problem Kind', max_length=4, choices=EPROB_PROBLEM_CHOICES)
	compl = models.CharField('Complex Numbers', max_length=1, choices=EPROB_YESNO_CHOICES)
	matrix = models.CharField('Matrix Type', max_length=3, choices=EPROB_MATRIX_CHOICES)
	storage = models.CharField('Matrix Storage', max_length=4, choices=EPROB_STORAGE_CHOICES, blank = True, null = True)
	schur = models.CharField('Schur Form', max_length=1, choices=EPROB_YESNO_CHOICES, blank = True, null = True)
	evaluerange = models.CharField('Range of Eigenvalues', max_length=1, choices=EPROB_YESNO_CHOICES, blank= True, null = True)
	algorithm = models.CharField('Algorithm Used', max_length=4, choices=EPROB_ALGORITHM_CHOICES, blank = True, null = True)
	balancing = models.CharField('Balancing Transform', max_length=1, choices=EPROB_YESNO_CHOICES, blank = True, null = True)
	schurform = models.CharField('Ordered Schur Form', max_length=1, choices=EPROB_YESNO_CHOICES, blank = True, null = True)
	precision = models.CharField('Number Precision', max_length=1, choices=EPROB_PRECISION_CHOICES)
	routineName = models.CharField('Routine Name', max_length=8)
	url = models.URLField('Function URL', blank = True, null = True)
	info = models.ForeignKey(lapack_eprob_RoutineInfo, primary_key=True)

	def __unicode__(self):
		return self.routineName
	
	class Meta:
		app_label = 'lighthouse'
