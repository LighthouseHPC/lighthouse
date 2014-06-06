import string, types, sys, os, StringIO, re, shlex, json, zipfile
from collections import OrderedDict
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

form_order = ('standardGeneralizedForm', 'complexNumberForm', 'matrixTypeForm', 'storageTypeForm', 'singleDoubleForm')

form_2arguments = ['matrixTypeForm',]      ## forms that require two arguments


### help functions
def find_nextForm(currentForm_name, request):   
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
def guidedSearch_index(request):
    ## set up session keys and values
    for item in form_order:
        key = 'sylvester_'+item[:-4]
        request.session[key] = ''    
    request.session['currentForm_name'] = 'standardGeneralizedForm'
    request.session['Routines'] = lapack_sylvester.objects.all()
    request.session['sylvester_guided_answered'] = OrderedDict()
    
    ## get ready for the template
    context = {
                'formHTML': "standardGeneralizedForm",
                'form': "invalid",
                'sylvester_guided_answered' : '',
                'results' : 'start'
    }
    return render_to_response('lighthouse/lapack_sylvester/index.html', context_instance=RequestContext(request, context))




def guidedSearch(request):
    ## distinguish forms that take 2 arguments from forms that take 1 argument
    if request.session['currentForm_name'] in form_2arguments:
        form = getattr(sys.modules[__name__], request.session['currentForm_name'])(request, request.GET or None)   #handle GET and POST in the same view
    else:
        form = getattr(sys.modules[__name__], request.session['currentForm_name'])(request.GET or None)
        
    if form.is_valid():
        ## get current question and user's answer
        current_question = request.session['currentForm_name'][:-4]
        formField_name = 'sylvester_'+current_question
        value = form.cleaned_data[formField_name]
        choices = form.fields[formField_name].choices        
        request.session['sylvester_guided_answered'].update(question_and_answer(form, value, choices))
        
        ## do search based on user's response
        lookup = "%s__contains" % current_question
        query = {lookup : value}
        request.session['Routines'] = request.session['Routines'].filter(**query)
        
        ## generate a session for current question/answer -->request.session[sylvester_currentQuestion] = answer
        request.session[formField_name] = value
        
        ## call function find_nextForm to set up next form for next question
        dict_nextQuestion = find_nextForm(request.session['currentForm_name'], request)           
        nextForm_name = dict_nextQuestion['nextForm_name']
        nextForm = dict_nextQuestion['nextForm']
        
        ## make next form current for request.session['currentForm_name']
        request.session['currentForm_name'] = nextForm_name 
        
        ## decide whether or not to use form HTML files (if help buttons are needed, use HTML file instead of form)
        if nextForm_name in ["standardGeneralizedForm"]:
            formHTML = nextForm_name
        else:
            formHTML = "invalid"
        
        ## get ready for the template       
        context = {
                    'formHTML': formHTML,
                    'form': nextForm,
                    'sylvester_guided_answered' : request.session['sylvester_guided_answered'],
                    'results' : request.session['Routines']
                    }
        return render_to_response('lighthouse/lapack_sylvester/index.html', context_instance=RequestContext(request, context))
    else:       
        return guidedSearch_index(request)
    






##############################################
######-------- Advanced Search -------- ######
##############################################