from django.db import models
from lighthouse.models.lapack_le import lapack_RoutineInfo

EPROB_PROBLEM_CHOICES = (
	(u'eval' 				,u'Eigenproblem'),
	(u'svd'					,u'Singular Value Decomposition')
)

EPROB_NOYES_CHOICES = (
	(u'n'	,			u'No'),
	(u'y'	,			u'Yes')
)

EPROB_YESNO_CHOICES = (
	(u'y'	,			u'Yes'),
	(u'n'	,			u'No')
)

EPROB_MATRIX_CHOICES = (
	(u'gen',			u'General'),
	(u'sym',			u'Symmetric'),
	(u'her',			u'Hermitian'),	
)

EPROB_STORAGE_CHOICES = (
	(u'full',			u'Full'),
	(u'band',			u'Band'),
	(u'pack',			u'Packed'),
	(u'tri',			u'Tridiagonal'),
)

EPROB_PRECISION_CHOICES = (
	(u's',			u'Single'),
	(u'd',			u'Double'),
)



EPROB_ALGORITHM_CHOICES = (
	(u'n', 			u'Default algorithm'),
	(u'dc',			u'Divide and Conquer'),
	(u'rrr',		u'Relatively Robust Representation'),
	(u'mrrr',		u'Multiple Relatively Robust Representation'),
)

class lapack_eprob_simple(models.Model):
	generalized = models.CharField('Generalized', max_length=1, choices=EPROB_YESNO_CHOICES)
	problem = models.CharField('Problem Kind', max_length=4, choices=EPROB_PROBLEM_CHOICES)
	complex = models.CharField('Complex Numbers', max_length=1, choices=EPROB_YESNO_CHOICES)
	matrix = models.CharField('Matrix Type', max_length=3, choices=EPROB_MATRIX_CHOICES)
	storage = models.CharField('Matrix Storage', max_length=4, choices=EPROB_STORAGE_CHOICES, blank = True, null = True)
	schur = models.CharField('Schur Form', max_length=1, choices=EPROB_YESNO_CHOICES, blank = True, null = True)
	evaluerange = models.CharField('Range of Eigenvalues', max_length=1, choices=EPROB_YESNO_CHOICES, blank= True, null = True)
	algorithm = models.CharField('Algorithm Used', max_length=4, choices=EPROB_ALGORITHM_CHOICES, blank = True, null = True)
	balancing = models.CharField('Balancing Transform', max_length=1, choices=EPROB_YESNO_CHOICES, blank = True, null = True)
	schurform = models.CharField('Ordered Schur Form', max_length=1, choices=EPROB_YESNO_CHOICES, blank = True, null = True)
	queryPrecision = models.CharField('Number Precision', max_length=1, choices=EPROB_PRECISION_CHOICES)
	precision = models.CharField('Number Precision', max_length=1)
	routineName = models.CharField('Routine Name', max_length=8)
	url = models.URLField('Function URL', blank = True, null = True)
	info = models.ForeignKey(lapack_RoutineInfo, primary_key=True)

	def __unicode__(self):
		return self.routineName
	
	class Meta:
		app_label = 'lighthouse'
