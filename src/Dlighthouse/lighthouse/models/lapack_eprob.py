from django.db import models
from django.db.models import Q
from lighthouse.models.lapack_le import lapack_RoutineInfo
from collections import OrderedDict

EPROB_PROBLEM_CHOICES = (
    (u'eval'                 ,u'Eigenproblem'),
    (u'svd'                  ,u'Singular Value Decomposition')
)

EPROB_NOYES_CHOICES = (
    (u'n'    ,u'No'),
    (u'y'    ,u'Yes')
)

EPROB_YESNO_CHOICES = (
    (u'y'    ,u'Yes'),
    (u'n'    ,u'No')
)

EPROB_MATRIX_CHOICES = (
    (u'gen',u'General'),
    (u'sym',u'Symmetric'),
    (u'her',u'Hermitian'),   
)

EPROB_STORAGE_CHOICES = (
    (u'full',u'Full'),
    (u'band',u'Band'),
    (u'pack',u'Packed'),
    (u'tri',u'Tridiagonal'),
)

EPROB_PRECISION_CHOICES = (
    (u's',u'Single'),
    (u'd',u'Double'),
)

EPROB_ALGORITHM_CHOICES = (
    (u'n',u'Default algorithm'),
    (u'dc',u'Divide and Conquer'),
    (u'rrr',u'Relatively Robust Representation'),
    (u'mrrr',u'Multiple Relatively Robust Representation'),
)

## ---------------- Define the forms ---------------- ###
eprob_fields = OrderedDict([
        ('generalized' , ('Do you need to solve a generalized eigenproblem?', EPROB_NOYES_CHOICES)),
        ('problem' , ('What type of problem do you need to solve?', EPROB_PROBLEM_CHOICES)),
        ('complex' , ('Does your matrix have any complex numbers?', EPROB_NOYES_CHOICES)),
        ('matrixType' , ('What type of matrix do you have?', EPROB_MATRIX_CHOICES)),
        ('storageType' , ('How is your matrix stored?', EPROB_STORAGE_CHOICES)),
        ('schur' , ('Do you need the Schur form?', EPROB_NOYES_CHOICES)),
        ('evaluerange' , ('Do you only need eigenvalues within a specific range?', EPROB_NOYES_CHOICES)),
        ('algorithm' , ('Do you need to use an advanced algorithm?', EPROB_ALGORITHM_CHOICES)),
        ('balancing' , ('Do you need a balancing transform or reciprocal condition numbers?', EPROB_NOYES_CHOICES)),
        ('schurform' , ('Do you need the ordered Schur form or reciprocal condition numbers?', EPROB_NOYES_CHOICES)),
        ('queryPrecision' , ('Is your matrix single or double precision?', EPROB_PRECISION_CHOICES)),
    ])

eprob_nextform = OrderedDict([
        ('start' , 'generalized'),
        ('generalized', 'problem'),
        ('problem', 'complex'),
        ('complex' , 'matrixType'),
        ('matrixType' , 'storageType'),
        ('storageType' , 'schur'),
        ('schur' , 'evaluerange'),
        ('evaluerange' , 'algorithm'),
        ('algorithm' , 'balancing'),
        ('balancing' , 'schurform'),
        ('schurform' , 'queryPrecision'),
        ('queryPrecision' , 'finish'),
    ])

eprob_advanced_landing = (
    'generalized', 
    'problem'
    )

eprob_advanced_questions = (
    'complex' , 
    'matrixType',
    'storageType',
    'queryPrecision',
    'schur',
)

eprob_advanced_optional = (
    'algorithm',
    'evaluerange',
    'balancing',
    'schurform',
)
eprob_advanced_forms = OrderedDict([
    ('landing', eprob_advanced_landing),
    ('questions', eprob_advanced_questions),
    ('optional', eprob_advanced_optional),
    ])

eprob_advanced_nextform = OrderedDict([
    ('start', 'landing'),
    ('landing', 'questions'),
    ('questions', 'optional'),
    ('optional', 'finish'),
    ])

class lapack_eprob(models.Model):
    generalized = models.CharField('Generalized', max_length=1, choices=EPROB_YESNO_CHOICES)
    problem = models.CharField('Problem Kind', max_length=4, choices=EPROB_PROBLEM_CHOICES)
    complex = models.CharField('Complex Numbers', max_length=1, choices=EPROB_YESNO_CHOICES)
    matrixType = models.CharField('Matrix Type', max_length=3, choices=EPROB_MATRIX_CHOICES)
    storageType = models.CharField('Matrix Storage', max_length=4, choices=EPROB_STORAGE_CHOICES, blank = True, null = True)
    schur = models.CharField('Schur Form', max_length=1, choices=EPROB_YESNO_CHOICES, blank = True, null = True)
    evaluerange = models.CharField('Range of Eigenvalues', max_length=1, choices=EPROB_YESNO_CHOICES, blank= True, null = True)
    algorithm = models.CharField('Algorithm Used', max_length=4, choices=EPROB_ALGORITHM_CHOICES, blank = True, null = True)
    balancing = models.CharField('Balancing Transform', max_length=1, choices=EPROB_YESNO_CHOICES, blank = True, null = True)
    schurform = models.CharField('Ordered Schur Form', max_length=1, choices=EPROB_YESNO_CHOICES, blank = True, null = True)
    queryPrecision = models.CharField('Number Precision', max_length=1, choices=EPROB_PRECISION_CHOICES)
    thePrecision = models.CharField('Number Precision', max_length=1)
    routineName = models.CharField('Routine Name', max_length=8)
    url = models.URLField('Function URL', blank = True, null = True)
    info = models.ForeignKey(lapack_RoutineInfo)

    def __unicode__(self):
        return self.routineName

    class Meta:
        app_label = 'lighthouse'


def getFilteredList(answered_questions = OrderedDict()):
    db = lapack_eprob.objects.all()
    Qr = None
    for field,values in answered_questions.items():
        q = Q(**{"%s__in" % field: values })
        if Qr:
            Qr = Qr & q # or & for filtering
        else:
            Qr = q    

    if Qr != None:
        db = db.filter(Qr)
    return db

def getFilteredChoices(filtered,field):
    result = ()
    possibleResults = filtered.values_list(field, flat=True).order_by(field).distinct()
    if field in eprob_fields:
        label, choices = eprob_fields[field]
        
        for shortAns, longAns in choices:
            if shortAns in possibleResults:
                result = result + ((shortAns,longAns),)
    return result

def getFilteredChoicesAdvanced(filtered,field):
    result = ()

    if field in eprob_advanced_forms:
        for i in eprob_advanced_forms[field]:
            choices = getFilteredChoices(filtered,i)
            if len(choices) > 1:
                label,_ = eprob_fields[i]
                result = result + ((i,label,choices),)

    return result

def findNextForm(filtered_list=lapack_eprob.objects.all(),answered=OrderedDict()):
    field = eprob_nextform['start']
    while field != 'finish':
        if field in eprob_fields:
            if field not in answered:
                # test to see if it has more than one possible value
                qnum = filtered_list.filter().values_list(field, flat=True).distinct().count()
                if qnum > 1:
                    break
            field = eprob_nextform[field]
        else:
            return 'finish'
    return field

def findNextFormAdvanced(filtered_list=lapack_eprob.objects.all(),answered=OrderedDict()):
        for temp in eprob_advanced_forms:
            viewed = False
            for field in eprob_advanced_forms[temp]:
                if field in answered:
                    viewed = True;
                    break
            if not viewed:
                for field in eprob_advanced_forms[temp]:
                    if field not in answered:
                        if field in eprob_fields:
                            # test to see if it has more than one possible value
                            qnum = filtered_list.filter().values_list(field, flat=True).distinct().count()
                            if qnum > 1:
                                return temp
            temp = eprob_advanced_nextform[temp]
        return 'finish'