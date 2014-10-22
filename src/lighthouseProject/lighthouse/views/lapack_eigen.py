import string, types, sys, os, StringIO, re, shlex, json, zipfile
from collections import OrderedDict
from django.contrib.auth.decorators import login_required
from django.core.servers.basehttp import FileWrapper
from django.http import HttpResponse, HttpResponseNotFound
from django.shortcuts import render_to_response, redirect, render
from django.template import RequestContext
from django.template.loader import render_to_string
from django.views.decorators.csrf import csrf_exempt
from lighthouse.forms.lapack_eigen import *
from lighthouse.models.lapack_eigen import lapack_eigen_guided
from lighthouse.models.lapack_choiceDict import *

import datetime



###############################################
######--------- Help Functions --------- ######
###############################################

### set up initial sessions
#def sessionSetup(request):
#    for item in ['problemForm', 'complexNumberForm', 'matrixTypeForm', 'storageTypeForm', 'singularVectorsForm', 'singleDoubleForm']:
#        key = 'eigen_'+item[:-4]
#        request.session[key] = ''    
#    request.session['currentForm_name'] = 'problemForm'
#    request.session['Routines'] = lapack_eigen.objects.all()
#    request.session['eigen_guided_answered'] = OrderedDict()
    
    
    
    
    
    

##############################################
######--------- Guided Search --------- ######
##############################################

form_order = ('problemForm', 'standardGeneralizedForm', 'complexNumberForm', 'matrixTypeForm', 'storageTypeForm',
                'selectedEVForm', 'eigenvectorForm', 'schurForm', 'cndNumberForm', 'singleDoubleForm')

form_2arguments = ['matrixTypeForm', 'storageTypeForm']     ## forms that require two arguments

form_HTML = ['standardGeneralizedForm']        ## forms with HTML format


### help functions
def question_and_answer(form, value, choices):
    for field in form:
        question = unicode(field.label)
    for choice in choices:
        if choice[0] == value:
            answer = choice[1]
    return {question: [answer]}
    



def find_nextForm(currentForm_name, request):
    ## eigen problem for "general matrix" is special when the answer to the eigenvector question is 'yes'
    ## the next question in this case is cndNumberForm
    if request.session['eigen_matrixType'] == 'general' and request.session['eigen_eigenvector'] == 'yes' and request.session['eigen_cndNumber'] == '':
        nextForm_name = 'cndNumberForm'
        nextForm = cndNumberForm()
    ## skip matrixTypeForm and storageTypeForm for 'Balance a general matrix to improve eigenvalue accuracy'
    elif request.session['eigen_problem'] == 'balance' and request.session['eigen_complexNumber'] != '' and request.session['eigen_singleDouble'] == '':
        nextForm_name = 'singleDoubleForm'
        nextForm = singleDoubleForm()
    else:   
        current_index = form_order.index(currentForm_name)
        nextForm_name = ""        
        nextForm = ""
        try:
            ## search for 'none' and return the first column that has zero to be the next question/form
            next_index = next(i for i in range(current_index+1, len(form_order)) if request.session['Routines'].filter(**{form_order[i][:-4]: 'none'}).count() == 0)
            nextForm_name = form_order[next_index]
            if nextForm_name in form_2arguments:
                nextForm = getattr(sys.modules[__name__], nextForm_name)(request)
            else:
                nextForm = getattr(sys.modules[__name__], nextForm_name)()
        ## the end of the guided search or other errors
        except Exception as e:          
            print type(e)
            print "e.message: ", e.message
            print "e.args: ", e.args
    
    return {'nextForm_name': nextForm_name, 'nextForm': nextForm}
    



### start guided search views
def index(request):
    ## set up session keys and values
    for item in form_order:
        key = 'eigen_'+item[:-4]
        request.session[key] = ''    
    request.session['currentForm_name'] = 'problemForm'
    request.session['Routines'] = lapack_eigen_guided.objects.all()
    request.session['eigen_guided_answered'] = OrderedDict()
    
    ## get ready for the template
    context = {
                'formHTML': "problemForm",
                'form': "invalid",
                'eigen_guided_answered' : '',
                'results' : 'start',
                'advancedSearchMenuForm': advancedSearchMenuForm(),
    }
    return render_to_response('lighthouse/lapack_eigen/index.html', context_instance=RequestContext(request, context))




def guidedSearch(request):
    ## distinguish forms that take 2 arguments from forms that take 1 argument
    if request.session['currentForm_name'] in form_2arguments:
        form = getattr(sys.modules[__name__], request.session['currentForm_name'])(request, request.GET or None)   #handle GET and POST in the same view
    else:
        form = getattr(sys.modules[__name__], request.session['currentForm_name'])(request.GET or None)
        
    if form.is_valid():
        ## get current question and user's answer
        current_question = request.session['currentForm_name'][:-4]
        formField_name = 'eigen_'+current_question
        value = form.cleaned_data[formField_name]
        choices = form.fields[formField_name].choices        
        request.session['eigen_guided_answered'].update(question_and_answer(form, value, choices))
 
        ## if user chooses to stop the search, start over; otherwise, perform the search
        if value == 'stop':
            return index(request)
        else:        
            ## do search based on user's response
            lookup = "%s__contains" % current_question
            query = {lookup : value}
            request.session['Routines'] = request.session['Routines'].filter(**query)
            
            ## generate a session for current question/answer -->request.session[eigen_currentQuestion] = answer
            request.session[formField_name] = value
            
            ## call function find_nextForm to set up next form for next question
            dict_nextQuestion = find_nextForm(request.session['currentForm_name'], request)           
            nextForm_name = dict_nextQuestion['nextForm_name']
            nextForm = dict_nextQuestion['nextForm']
            
            ## make next form current for request.session['currentForm_name']
            request.session['currentForm_name'] = nextForm_name 
            
            ## decide whether or not to use form HTML files (if help buttons are needed, use HTML file instead of form)
            if nextForm_name in form_HTML:
                formHTML = nextForm_name
            else:
                formHTML = "invalid"
            
            ## get ready for the template       
            context = {
                        'formHTML': formHTML,
                        'form': nextForm,
                        'eigen_guided_answered' : request.session['eigen_guided_answered'],
                        'results' : request.session['Routines']
                        }
            return render_to_response('lighthouse/lapack_eigen/index.html', context_instance=RequestContext(request, context))
    else:       
        return index(request)
    



### ------------------------- Note: question order for different problem types in the guided search ------------------------###
#(1) eigen problem 
#    (a)'symmetric':            ['standardGeneralized', 'complexNumber', 'matrixType', 'storageType', 'selectedEV', 'eigenvector',                      'singleDouble']
#    (b)'Hermitian':            ['standardGeneralized', 'complexNumber', 'matrixType', 'storageType', 'selectedEV', 'eigenvector',                      'singleDouble']
#    (c)'SPD':                  ['standardGeneralized', 'complexNumber', 'matrixType', 'storageType',               'eigenvector',                      'singleDouble']
#    (d)'HPD':                  ['standardGeneralized', 'complexNumber', 'matrixType', 'storageType',               'eigenvector',                      'singleDouble']    
#    (e)'upper Hessenberg':     ['standardGeneralized', 'complexNumber', 'matrixType', 'storageType',               'eigenvector',                      'singleDouble']
#    (f)'general':              ['standardGeneralized', 'complexNumber', 'matrixType', 'storageType',               'eigenvector', 'schur', 'cndNumber','singleDouble']   
#(2) upper Hessenberg:          ['standardGeneralized', 'complexNumber', 'matrixType', 'storageType',                                                   'singleDouble']
#(3) generalized_to_standard:   [                       'complexNumber', 'matrixType', 'storageType',                                                   'singleDouble']
#(4) conditionNumber:           ['standardGeneralized', 'complexNumber', 'matrixType',                                                                  'singleDouble']
#(5) balance:                   ['standardGeneralized', 'complexNumber',                                                                                'singleDouble']
### ------------------------------------------------------------------------------------------------------------------------###




##############################################
######-------- Advanced Search -------- ######
##############################################
def advancedForm(request):
    request.session['advancedForms'] = []
    request.session['formDict'] = {}                                ##store formNames and corresponding fields --> {formName: [list, of, fields]}  
    request.session['col_no'] = [1,2,3,4,5,6,7,8]                   ## col_no is the column(s) to hide
    form_col = {
        'driver_standard_sh': 1,
        'driver_standard_g': 2,
        'driver_generalized_sh': 3,
        'driver_generalized_g': 4,
        'computational_standard_sh': 5,
        'computational_standard_g': 6,
        'computational_generalized_sh': 7,
        'computational_generalized_g': 8
    }
        
    form = advancedSearchMenuForm(request.POST or None)
    if form.is_valid():
        value = form.cleaned_data['advancedSearchMenu']
        for item in value:
            request.session['advancedForms'].append(item)           ## store selected forms for displaying bound forms
            request.session['col_no'].remove(form_col[item])        ## remove column(s) that are needed from col_no list
            
            ## construct request.session['formDict'] for form validation
            fieldList = []
            itemForm = getattr(sys.modules[__name__], item+'_Form')()
            for index, value in itemForm.fields.items():
                fieldList.append(index)
            request.session['formDict'][item] = fieldList

        ## context includes guided search form    
        context = {
            'AdvancedTab': True,
            'form1': driver_standard_sh_Form(),
            'form2': driver_standard_g_Form(),
            'form3': driver_generalized_sh_Form(),
            'form4': driver_generalized_g_Form(),
            'form5': computational_standard_sh_Form(),
            'form6': computational_standard_g_Form(),
            'form7': computational_generalized_sh_Form(),
            'form8': computational_generalized_g_Form(),
            'formDict': request.session['formDict'],
            'results': 'start',
            'col_no': request.session['col_no'],
            'formHTML': "problemForm",
            'form': "invalid",
            'eigen_guided_answered' : '',
        }
        return render_to_response('lighthouse/lapack_eigen/index.html', context_instance=RequestContext(request, context))
    
    else:
        return index(request)
    
    

## for an arbitrary number of loops        
def multi_for(iterables):
    if not iterables:
         yield ()
    else:
        for item in iterables[0]:
            for rest_tuple in multi_for(iterables[1:]):
                yield (item,) + rest_tuple
                
                
                
        
def advancedSearch(request):
    request.session['advancedResults'] = []
    for item in request.session['advancedForms']:
        form = getattr(sys.modules[__name__], item+'_Form')(request.POST or None)
        queryDict2 = {}
        kwargs = {}
        modelFieldList = []
        rangeList = []
        if form.is_valid():
            for index, value in form.fields.items():
                queryDict2[index.split('_')[-1]] = form.cleaned_data[index]
                rangeList.append(len(form.cleaned_data[index]))
                modelFieldList.append(index.split('_')[-1])
            
            for i in multi_for(map(xrange, rangeList)):
                kwargs = {'driverComput': item.split('_')[0], 'standardGeneralized': item.split('_')[1]}
                for model, answer in zip(modelFieldList, i):
                    kwargs.update({model: queryDict2[model][answer]})
                    
                request.session['advancedResults'].extend(lapack_eigen_advanced.objects.filter(**kwargs))

    ## be ready for switching to guided search
    #sessionSetup(request)
    
    context = {
            'AdvancedTab': True,
            'form_submitted': 'yes',
            'form1': driver_standard_sh_Form(request.POST or None),
            'form2': driver_standard_g_Form(request.POST or None),
            'form3': driver_generalized_sh_Form(request.POST or None),
            'form4': driver_generalized_g_Form(request.POST or None),
            'form5': computational_standard_sh_Form(request.POST or None),
            'form6': computational_standard_g_Form(request.POST or None),
            'form7': computational_generalized_sh_Form(request.POST or None),
            'form8': computational_generalized_g_Form(request.POST or None),
            'formDict': request.session['formDict'],
            'results': request.session['advancedResults'],
            'col_no': request.session['col_no'],
            'formHTML': "problemForm",
            'form': "invalid",
            'eigen_guided_answered' : '',
    }
    return render_to_response('lighthouse/lapack_eigen/index.html', context_instance=RequestContext(request, context))    