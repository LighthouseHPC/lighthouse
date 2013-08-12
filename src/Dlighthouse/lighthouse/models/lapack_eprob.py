from django.db import models
from django.db.models import Q
from lighthouse.models.lapack_le import lapack_RoutineInfo
from collections import OrderedDict

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

## ---------------- Define the forms ---------------- ###
eprob_fields = OrderedDict([
        ('problem' , ('What kind of eigenproblem do you want to solve?', EPROB_PROBLEM_CHOICES)),
        ('complex' , ('Does your matrix contain any complex numbers?', EPROB_NOYES_CHOICES)),
        ('matrix' , ('What type of matrix do you have?', EPROB_MATRIX_CHOICES)),
        ('storage' , ('How is your matrix stored?', EPROB_STORAGE_CHOICES)),
        ('schur' , ('Do you need the Schur form?', EPROB_NOYES_CHOICES)),
        ('evaluerange' , ('Do you only need eigenvalues within a specific range?', EPROB_NOYES_CHOICES)),
        ('algorithm' , ('Do you need to use a specific algorithm?', EPROB_ALGORITHM_CHOICES)),
        ('balancing' , ('Do you need a balancing transform or reciprocal condition numbers?', EPROB_NOYES_CHOICES)),
        ('schurform' , ('Do you need the ordered Schur form or reciprocal condition numbers?', EPROB_NOYES_CHOICES)),
        ('queryPrecision' , ('Is your matrix single or double precision?', EPROB_PRECISION_CHOICES)),
    ])



eprob_nextform = OrderedDict([
        ('start' , 'problem'),
        ('problem', 'complex'),
        ('complex' , 'matrix'),
        ('matrix' , 'storage'),
        ('storage' , 'schur'),
        ('schur' , 'evaluerange'),
        ('evaluerange' , 'algorithm'),
        ('algorithm' , 'balancing'),
        ('balancing' , 'schurform'),
        ('schurform' , 'queryPrecision'),
        ('queryPrecision' , 'finish'),
    ])

def getFilteredList(answered_questions):
    db = lapack_eprob_simple.objects.all()
    Qr = None
    for field,value in answered_questions.items():
        q = Q(**{"%s__exact" % field: value })
        if Qr:
            Qr = Qr & q # or & for filtering
        else:
            Qr = q    

    if Qr != None:
        db = db.filter(Qr)
    return db

def findNextForm(filtered_list,last):
	try:
	    field = eprob_nextform[last]
	except KeyError:
	    return 'finish'
	num = filtered_list.count()
	while field != 'finish':
	    (value,_) = eprob_fields[field][1][0]
	    qnum = filtered_list.filter(Q(**{"%s__exact"% field : value})).count()
	    if (num != qnum and qnum > 0):
	        break
	    field = eprob_nextform[field]
	return field

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
	info = models.ForeignKey(lapack_RoutineInfo)

	def __unicode__(self):
		return self.routineName

	class Meta:
		app_label = 'lighthouse'
