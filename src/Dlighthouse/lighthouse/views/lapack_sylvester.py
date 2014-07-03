import string, types, sys, os, StringIO, re, shlex, json, zipfile
from collections import OrderedDict
from itertools import chain
from django.contrib.auth.decorators import login_required
from django.core.servers.basehttp import FileWrapper
from django.http import HttpResponse, HttpResponseNotFound
from django.shortcuts import render_to_response, redirect, render
from django.template import RequestContext
from django.template.loader import render_to_string
from django.views.decorators.csrf import csrf_exempt
from lighthouse.forms.lapack_sylvester import *
from lighthouse.models.lapack_sylvester import lapack_sylvester
from lighthouse.models.lapack_choiceDict import *
from lighthouse.views.lapack_eigen import question_and_answer

import datetime

##############################################
######--------- Guided Search --------- ######
##############################################


form_order_standard = ('standardGeneralizedForm', 'complexNumberForm', 'standardConditionForm', 'singleDoubleForm')         ## form order for standard Sylvester equation
form_order_generalized = ('standardGeneralizedForm', 'complexNumberForm', 'generalizedConditionForm', 'singleDoubleForm')   ## form order for generalized Sylvester equation

form_HTML = ['standardGeneralizedForm', 'standardConditionForm', 'generalizedConditionForm']                                ## forms with HTML format


### help functions
def find_nextForm(currentForm_name, request):
    print request.session['form_order']
    current_index = request.session['form_order'].index(currentForm_name)
    nextForm_name = ""        
    nextForm = ""
    
    try: 
        ## search for 'none' and return the first column that has zero to be the next question/form
        next_index = next(i for i in range(current_index+1, len(request.session['form_order'])))
        nextForm_name = request.session['form_order'][next_index]
        print nextForm_name
        nextForm = getattr(sys.modules[__name__], nextForm_name)()
    ## the end of the guided search or other errors
    except Exception as e:          
        print type(e)
        print "e.message: ", e.message
        print "e.args: ", e.args
    
    return {'nextForm_name': nextForm_name, 'nextForm': nextForm}
    

### set up initial sessions
def sessionSetup(request):
    for item in ['standardGeneralizedForm', 'standardConditionForm', 'generalizedConditionForm', 'complexNumberForm', 'singleDoubleForm']:
        key = 'sylvester_'+item[:-4]
        request.session[key] = ''
    request.session['currentForm_name'] = 'standardGeneralizedForm'
    request.session['Results'] = lapack_sylvester.objects.all()
    request.session['sylvester_guided_answered'] = OrderedDict()
    request.session['form_order'] = []




### start guided search views
def index(request):
    # set up session keys and values
    sessionSetup(request)
    
    ## get ready for the template
    context = {
                'formHTML': "standardGeneralizedForm",
                'form': "invalid",
                'sylvester_guided_answered' : '',
                'results' : 'start',
    }
    return render_to_response('lighthouse/lapack_sylvester/index.html', context_instance=RequestContext(request, context))




def guidedSearch(request):
    form = getattr(sys.modules[__name__], request.session['currentForm_name'])(request.GET or None)
        
    if form.is_valid():
        ## get current question and user's answer
        current_question = request.session['currentForm_name'][:-4]
        formField_name = 'sylvester_'+current_question
        value = form.cleaned_data[formField_name]
        choices = form.fields[formField_name].choices        
        request.session['sylvester_guided_answered'].update(question_and_answer(form, value, choices))

        ## generate a session for current question/answer -->request.session[sylvester_currentQuestion] = answer
        request.session[formField_name] = value
        
        
        ## decide which form order to use
        if request.session['currentForm_name'] == 'standardGeneralizedForm' and request.session['sylvester_standardGeneralized'] == 'standard':
            request.session['form_order'] = form_order_standard
        elif request.session['currentForm_name'] == 'standardGeneralizedForm' and request.session['sylvester_standardGeneralized'] == 'generalized':
            request.session['form_order'] = form_order_generalized

        
        if request.session['sylvester_standardCondition'] == 'no' or request.session['sylvester_generalizedCondition'] == 'no':         ## stop search
            return index(request)
        else:
            ## do search based on user's response (no search needed for 'standardConditionForm', 'generalizedConditionForm')
            if request.session['currentForm_name'] not in ['standardConditionForm', 'generalizedConditionForm']:
                lookup = "%s__contains" % current_question
                query = {lookup : value}
                request.session['Results'] = request.session['Results'].filter(**query)
            
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
                        'sylvester_guided_answered' : request.session['sylvester_guided_answered'],
                        'results' : request.session['Results']
                        }
            return render_to_response('lighthouse/lapack_sylvester/index.html', context_instance=RequestContext(request, context))
    else:       
        return index(request)
    






##############################################
######-------- Advanced Search -------- ######
##############################################
def advancedSearch(request):
    standardDict = {'complexNumber':[], 'singleDouble':[]}
    generalizedDict = {'complexNumber':[], 'singleDouble':[]}
    request.session['advancedResults'] = []
    form = advancedForm(request.POST or None) 

    ### search for standard routines
    if form['standard_search'].value() == 'yes':
        ## get standard data
        standardDict['complexNumber'] = form['standard_complexNumber'].value()
        standardDict['singleDouble'] = form['standard_singleDouble'].value()
        
        ## search for standard routines
        for item1 in standardDict['complexNumber']:
            for item2 in standardDict['singleDouble']:
                kwargs = {
                    'standardGeneralized': 'standard',
                    'complexNumber': item1,
                    'singleDouble': item2,
                }
                request.session['advancedResults'].extend(lapack_sylvester.objects.filter(**kwargs))

    ### search for generalized routines
    if form['generalized_search'].value() == 'yes':    
        ## get generalized data
        generalizedDict['complexNumber'] = form['generalized_complexNumber'].value()
        generalizedDict['singleDouble'] = form['generalized_singleDouble'].value()
        
        ## search for generalized routines        
        for item1 in generalizedDict['complexNumber']:
            for item2 in generalizedDict['singleDouble']:
                print item1, item2
                kwargs = {
                    'standardGeneralized': 'generalized',
                    'complexNumber': item1,
                    'singleDouble': item2,
                }
                request.session['advancedResults'].extend(lapack_sylvester.objects.filter(**kwargs))
    
    ## be ready for switching to guided search
    sessionSetup(request)
    
    ## context includes guided search form    
    context = {
        'form_submitted': form,
        'results': request.session['advancedResults'],
        'AdvancedTab': True,
        'formHTML': "standardGeneralizedForm",
        'form': "invalid",
        'sylvester_guided_answered' : '',
    }
    return render_to_response('lighthouse/lapack_sylvester/index.html', context_instance=RequestContext(request, context))
