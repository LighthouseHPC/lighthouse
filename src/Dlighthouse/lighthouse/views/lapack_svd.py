import string, types, sys, os, StringIO, re, shlex, json, zipfile
from collections import OrderedDict
from django.contrib.auth.decorators import login_required
from django.core.servers.basehttp import FileWrapper
from django.http import HttpResponse, HttpResponseNotFound
from django.shortcuts import render_to_response, redirect, render
from django.template import RequestContext
from django.template.loader import render_to_string
from django.views.decorators.csrf import csrf_exempt
from lighthouse.forms.lapack_svd import *
from lighthouse.models.lapack_svd import lapack_svd
from lighthouse.models.lapack_choiceDict import *
from lighthouse.views.lapack_eigen import question_and_answer

import datetime

##############################################
######--------- Guided Search --------- ######
##############################################

form_order = ('problemForm', 'complexNumberForm', 'matrixTypeForm', 'storageTypeForm', 'singularVectorsForm', 'singleDoubleForm')

form_2arguments = ['matrixTypeForm', 'storageTypeForm', 'singularVectorsForm']      ## forms that require two arguments

form_HTML = ['standardGeneralizedForm']        ## forms with HTML format


### help functions
def find_nextForm(currentForm_name, request):   
    current_index = form_order.index(currentForm_name)
    nextForm = ""
    try:
        ## count the number of distinct values in the filed and return the first column that has 2 or more distinct values to be the next question/form
        next_index = next(i for i in range(current_index+1, len(form_order)) if request.session['Routines'].values(form_order[i][:-4]).distinct().count() > 1)
        nextForm_name = form_order[next_index]
        if nextForm_name in form_2arguments:
            nextForm = getattr(sys.modules[__name__], nextForm_name)(request)
        else:
            nextForm = getattr(sys.modules[__name__], nextForm_name)()
    except Exception as e:          
        print type(e)
        print "e.message: ", e.message
        print "e.args: ", e.args
    
    return {'nextForm_name': nextForm_name, 'nextForm': nextForm}
    



### start guided search views
def index(request):
    ## set up session keys and values
    for item in ['problemForm', 'complexNumberForm', 'matrixTypeForm', 'storageTypeForm', 'singularVectorsForm', 'singleDoubleForm']:
        key = 'svd_'+item[:-4]
        request.session[key] = ''    
    request.session['currentForm_name'] = 'problemForm'
    request.session['Routines'] = lapack_svd.objects.all()
    request.session['svd_guided_answered'] = OrderedDict()  
    request.session['advancedResults'] = []
    
    ## get ready for the template
    context = {
                'formHTML': "problemForm",
                'form': "invalid",
                'svd_guided_answered' : '',
                'results' : 'start',
                'advancedSearchMenuForm': advancedSearchMenuForm(),
    }
    return render_to_response('lighthouse/lapack_svd/index.html', context_instance=RequestContext(request, context))




def guidedSearch(request):        
    ## distinguish forms that take 2 arguments from forms that take 1 argument
    if request.session['currentForm_name'] in form_2arguments:
        form = getattr(sys.modules[__name__], request.session['currentForm_name'])(request, request.GET or None)   #handle GET and POST in the same view
    else:
        form = getattr(sys.modules[__name__], request.session['currentForm_name'])(request.GET or None)
        
    if form.is_valid():
        ## get current question and user's answer
        current_question = request.session['currentForm_name'][:-4]
        formField_name = 'svd_'+current_question
        value = form.cleaned_data[formField_name]
        choices = form.fields[formField_name].choices        
        request.session['svd_guided_answered'].update(question_and_answer(form, value, choices))
        
        ## if user chooses to stop the search, start over; otherwise, perform the search
        if value == 'stop':
            return index(request)
        else:
            ## do search based on user's response
            lookup = "%s__contains" % current_question
            query = {lookup : value}
            request.session['Routines'] = request.session['Routines'].filter(**query)
            
            ## generate a session for current question/answer -->request.session[svd_currentQuestion] = answer
            request.session[formField_name] = value
            
            ## call function find_nextForm to set up next form for next question
            if request.session['currentForm_name'] == 'singleDoubleForm':
                nextForm_name = ''
                nextForm = ''
            else:
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
                        'svd_guided_answered' : request.session['svd_guided_answered'],
                        'results' : request.session['Routines']
                        }
            return render_to_response('lighthouse/lapack_svd/index.html', context_instance=RequestContext(request, context))
    else:       
        return index(request)
    






##############################################
######-------- Advanced Search -------- ######
##############################################
def advancedForm(request):
    request.session['advancedForms'] = []
    form = advancedSearchMenuForm(request.POST or None)   
    formList = []
    request.session['col_no'] = [1,2,3,4]      ## col_no is the column(s) to hide
    form_col = {'driver_standard': 1, 'driver_generalized': 2, 'computational_standard': 3, 'computational_generalized': 4}     
    if form.is_valid():
        value = form.cleaned_data['advancedSearchMenu']
        for item in value:
            request.session['advancedForms'].append(item)
            request.session['col_no'].remove(form_col[item])       ## remove the column(s) needed from col_no
            formList.append(getattr(sys.modules[__name__], item+'_Form')())         ## convert unicode to class
          
        ## context includes guided search form    
        context = {
            'AdvancedTab': True,
            'results': 'start',
            'formHTML': "problemForm",
            'form': "invalid",
            'svd_guided_answered' : '',
            'form1': driver_standard_Form(),
            'form2': driver_generalized_Form(),
            'form3': computational_standard_Form(),
            'form4': computational_generalized_Form(),
            'formList': formList,
            'col_no': request.session['col_no'],
        }
        return render_to_response('lighthouse/lapack_svd/index.html', context_instance=RequestContext(request, context))
    
    else:
        return index(request)
        
        
        
        
        
def advancedSearch(request):
    driver_standardDict = {'driverComput': 'driver', 'standardGeneralized': 'standard', 'complexNumber':[], 'method': [], 'singularVectors': [], 'singleDouble':[]}
    driver_generalizedDict = {'driverComput': 'driver', 'standardGeneralized': 'generalized', 'complexNumber':[], 'singularVectors': [], 'singleDouble':[]}
    computational_standardDict = {'driverComput': 'computational', 'standardGeneralized': 'standard', 'function': [], 'complexNumber':[], 'method': [], 'singularVectors': [], 'singleDouble':[]}
    computational_generalizedDict = {'driverComput': 'computational', 'standardGeneralized': 'generalized', 'function': [], 'complexNumber':[], 'singularVectors': [], 'singleDouble':[]}    
    request.session['advancedResults'] = []
    
    for item in request.session['advancedForms']:
        form = getattr(sys.modules[__name__], item+'_Form')(request.POST or None)
        if form.is_valid():
            queryDict1 = {'driverComput': item.split('_')[0], 'standardGeneralized': item.split('_')[1]}
            queryDict2 = {}
            kwargs = {}
            for index, value in form.fields.items():
                queryDict2[index.split('_')[-1]] = form.cleaned_data[index]
            print queryDict2
            
            for index, value in queryDict2.items():
                for item in value:
                    kwargs[index] = item
                    print queryDict2.iterkeys()
            
        context = {
                'AdvancedTab': True,
                'results': 'start',
                'formHTML': "problemForm",
                'form': "invalid",
                'svd_guided_answered' : '',
                'form1': driver_standard_Form(request.POST or None),
                'form2': driver_generalized_Form(request.POST or None),
                'form3': computational_standard_Form(request.POST or None),
                'form4': computational_generalized_Form(request.POST or None),
                #'formList': formList,
                'col_no': request.session['col_no'],
                #'form_submitted': form,
        }
        return render_to_response('lighthouse/lapack_svd/index.html', context_instance=RequestContext(request, context))