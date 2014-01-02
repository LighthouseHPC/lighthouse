from django.db import models
from django.db.models import Q
from lighthouse.models.lapack_le import lapack_RoutineInfo
from collections import OrderedDict


###<<<<< Guided Search >>>>>###
###--- Define possible choices for forms and database fields ---###

EIGEN_PROBLEM_CHOICES = (
    (u'eval'                 ,u'Eigenproblem'),
    (u'svd'                  ,u'Singular Value Decomposition')
)

EIGEN_NOYES_CHOICES = (
    (u'n'    ,u'No'),
    (u'y'    ,u'Yes')
)

EIGEN_YESNO_CHOICES = (
    (u'y'    ,u'Yes'),
    (u'n'    ,u'No')
)

EIGEN_MATRIX_CHOICES = (
    (u'gen',u'General'),
    (u'sym',u'Symmetric'),
    (u'her',u'Hermitian'),   
)

EIGEN_STORAGE_CHOICES = (
    (u'full',u'Full'),
    (u'band',u'Band'),
    (u'pack',u'Packed'),
    (u'tri',u'Tridiagonal'),
)

EIGEN_PRECISION_CHOICES = (
    (u's',u'Single'),
    (u'd',u'Double'),
)

EIGEN_ALGORITHM_CHOICES = (
    (u'n',u'Default algorithm'),
    (u'dc',u'Divide and Conquer'),
    (u'rrr',u'Relatively Robust Representation'),
    (u'mrrr',u'Multiple Relatively Robust Representation'),
)



###--- Define the model using the fields and choices from above ---###

class lapack_eigen(models.Model):
    generalized = models.CharField('Generalized', max_length=1, choices=EIGEN_YESNO_CHOICES)
    problem = models.CharField('Problem Kind', max_length=4, choices=EIGEN_PROBLEM_CHOICES)
    complex = models.CharField('Complex Numbers', max_length=1, choices=EIGEN_YESNO_CHOICES)
    matrixType = models.CharField('Matrix Type', max_length=3, choices=EIGEN_MATRIX_CHOICES)
    storageType = models.CharField('Matrix Storage', max_length=4, choices=EIGEN_STORAGE_CHOICES, blank = True, null = True)
    schur = models.CharField('Schur Form', max_length=1, choices=EIGEN_YESNO_CHOICES, blank = True, null = True)
    evaluerange = models.CharField('Range of Eigenvalues', max_length=1, choices=EIGEN_YESNO_CHOICES, blank= True, null = True)
    algorithm = models.CharField('Algorithm Used', max_length=4, choices=EIGEN_ALGORITHM_CHOICES, blank = True, null = True)
    balancing = models.CharField('Balancing Transform', max_length=1, choices=EIGEN_YESNO_CHOICES, blank = True, null = True)
    schurform = models.CharField('Ordered Schur Form', max_length=1, choices=EIGEN_YESNO_CHOICES, blank = True, null = True)
    queryPrecision = models.CharField('Number Precision', max_length=1, choices=EIGEN_PRECISION_CHOICES)
    thePrecision = models.CharField('Number Precision', max_length=1)
    routineName = models.CharField('Routine Name', max_length=8)
    url = models.URLField('Function URL', blank = True, null = True)
    info = models.ForeignKey(lapack_RoutineInfo)

    def __unicode__(self):
        return self.routineName

    class Meta:
        app_label = 'lighthouse'
        
        
        


###--- Define the possible fields in the database and in forms ---##

eigen_fields = OrderedDict([
        ('generalized' , ('Do you need to solve a generalized eigenproblem?', EIGEN_NOYES_CHOICES)),
        ('problem' , ('What type of problem do you need to solve?', EIGEN_PROBLEM_CHOICES)),
        ('complex' , ('Does your matrix have any complex numbers?', EIGEN_NOYES_CHOICES)),
        ('matrixType' , ('What type of matrix do you have?', EIGEN_MATRIX_CHOICES)),
        ('storageType' , ('How is your matrix stored?', EIGEN_STORAGE_CHOICES)),
        ('schur' , ('Do you need the Schur form?', EIGEN_NOYES_CHOICES)),
        ('evaluerange' , ('Do you only need eigenvalues within a specific range?', EIGEN_NOYES_CHOICES)),
        ('algorithm' , ('Do you need to use an advanced algorithm?', EIGEN_ALGORITHM_CHOICES)),
        ('balancing' , ('Do you need a balancing transform or reciprocal condition numbers?', EIGEN_NOYES_CHOICES)),
        ('schurform' , ('Do you need the ordered Schur form or reciprocal condition numbers?', EIGEN_NOYES_CHOICES)),
        ('queryPrecision' , ('Is your matrix single or double precision?', EIGEN_PRECISION_CHOICES)),
    ])



###--- Define the order the questions are asked in the guided search ---###

eigen_nextform = OrderedDict([
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





###--- Help Functions ---###

# return a QuerySet containing the results from a given set of questions
def getFilteredList(answered_questions = OrderedDict()):
    db = lapack_eigen.objects.all()
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


# Query the model to find what choices are available for a specific field given a set of results
def getFilteredChoices(results,field):
    result = ()
    possibleResults = results.values_list(field, flat=True).order_by(field).distinct()
    if field in eigen_fields:
        label, choices = eigen_fields[field]
        
        for shortAns, longAns in choices:
            if shortAns in possibleResults:
                result = result + ((shortAns,longAns),)
    return result


# Query the model to find the next field with more than one choice available
def findNextForm(filtered_list=lapack_eigen.objects.all(),answered=OrderedDict()):
    field = eigen_nextform['start']
    while field != 'finish':
        if field in eigen_fields:
            if field not in answered:
                # test to see if it has more than one possible value
                qnum = filtered_list.filter().values_list(field, flat=True).distinct().count()
                if qnum > 1:
                    break
            field = eigen_nextform[field]
        else:
            return 'finish'
    return field







###<<<<< Advanced Search >>>>>###
###--- Define the different pages for the advanced search ---###

eigen_advanced_landing = (
    'generalized', 
    'problem'
    )

eigen_advanced_questions = (
    'complex' , 
    'matrixType',
    'storageType',
    'queryPrecision',
    'schur',
)

eigen_advanced_optional = (
    'algorithm',
    'evaluerange',
    'balancing',
    'schurform',
)

eigen_advanced_forms = OrderedDict([
    ('landing', eigen_advanced_landing),
    ('questions', eigen_advanced_questions),
    ('optional', eigen_advanced_optional),
    ])

#
#
# Define the order the pages go in for the advanced search
#
#

eigen_advanced_nextform = OrderedDict([
    ('start', 'landing'),
    ('landing', 'questions'),
    ('questions', 'optional'),
    ('optional', 'finish'),
    ])




###--- Help Functions ---###

# Query the model to find what choices are available for a specific page given a set of results
def getFilteredChoicesAdvanced(results,field):
    result = ()

    if field in eigen_advanced_forms:
        for i in eigen_advanced_forms[field]:
            choices = getFilteredChoices(results,i)
            if len(choices) > 1:
                label,_ = eigen_fields[i]
                result = result + ((i,label,choices),)

    return result


# Query the model to find the next page with more than one choice available
def findNextFormAdvanced(filtered_list=lapack_eigen.objects.all(),answered=OrderedDict()):
        for temp in eigen_advanced_forms:
            viewed = False
            # loop through the fields in the page to see if any have been answered
            for field in eigen_advanced_forms[temp]:
                if field in answered:
                    viewed = True;
                    break

            # if none were answered, check to see if any of the forms are applicable
            if not viewed:
                for field in eigen_advanced_forms[temp]:
                    if field not in answered:
                        if field in eigen_fields:
                            # test to see if it has more than one possible value
                            qnum = filtered_list.filter().values_list(field, flat=True).distinct().count()
                            if qnum > 1:
                                return temp
            # go to the next page
            temp = eigen_advanced_nextform[temp]
        return 'finish'