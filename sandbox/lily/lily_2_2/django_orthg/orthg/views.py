import string, types, sys, os, StringIO, re, shlex, json, zipfile
from collections import OrderedDict
from django.contrib.auth.decorators import login_required
from django.core.servers.basehttp import FileWrapper
from django.http import HttpResponse, HttpResponseNotFound
from django.shortcuts import render_to_response, redirect, render
from django.template import RequestContext
from django.template.loader import render_to_string
from django.views.decorators.csrf import csrf_exempt
#from django.utils import simplejson 
from orthg.models import orthg
from orthg.choiceDict import *
from orthg.orthg_form import *

import datetime

form_order = ['standardGeneralizedForm', 'FullStorageForm', 'complexNumberForm', 'sFullRankForm', 'QRForm', 'SVDForm', 'gFullRankForm', 'singleDoubleForm']
form_rank = ['sFullRankForm']
form_HTML = ['standardGeneralizedForm']   
form_qrsvd = ['QRForm','SVDForm']

### help functions
def question_and_answer(form, value, choices):
    for field in form:
        question = unicode(field.label)
    for choice in choices:
        if choice[0] == value:
            answer = choice[1]
    return {question: [answer]}

def find_nextForm(currentForm_name, request):
    ## orthogonal problem for "general matrix" when the answer to the eigenvector question is 'yes'
    ## the next question in this case is gFullRankForm
    if request.session['orthg_standardGeneralized'] == 'generalized' and request.session['orthg_FullStorage'] == 'yes' and request.session['orthg_complexNumber'] and request.session['orthg_gFullRank'] == '':
        nextForm_name = 'gFullRankForm'
        nextForm = gFullRankForm()
    ## orthogonal problem for "general matrix" when the answer to the eigenvector question is 'yes'
    ## the next question in this case is sFullRankForm
    elif request.session['orthg_standardGeneralized'] == 'standard' and request.session['orthg_FullStorage'] == 'yes' and request.session['orthg_complexNumber'] and request.session['orthg_sFullRank'] == '':
         nextForm_name = 'sFullRankForm'
         nextForm = sFullRankForm()
    elif request.session['orthg_FullStorage'] == 'no' and request.session['orthg_complexNumber'] == '':
         nextForm_name = 'standardGeneralizedForm'
         nextForm = standardGeneralizedForm()
    elif request.session['orthg_sFullRank'] == 'no' and request.session['orthg_QR'] == '':
         nextForm_name = 'QRForm'
         nextForm = QRForm()
#    elif request.session['orthg_sFullRank'] == 'yes' and request.session['orthg_SVD'] == ''and request.session['orthg_QR'] == '': 
#         nextForm_name = 'SVDForm'
#         nextForm = SVDForm()
    elif (request.session['orthg_QR'] or request.session['orthg_SVD']) and request.session['orthg_singleDouble'] == '':
         nextForm_name = 'singleDoubleForm'
         nextForm = singleDoubleForm()

    else:   
        current_index = form_order.index(currentForm_name)
        nextForm_name = ""        
        nextForm = ""
        try:
            ## search for 'none' and return the first column that has zero to be the next question/form
            next_index = next(i for i in range(current_index+1, len(form_order))) 
            #FIX!!   if request.session['Routines'].filter(**{form_order[i][:-4]: 'none'}).count() == 0)
            nextForm_name = form_order[next_index]
#            if nextForm_name in form_rank:
#                 nextForm = getattr(sys.modules[__name__], nextForm_name)(request)
#            elif nextForm_name in form_qrsvd:
#                 nextForm = getattr(sys.modules[__name__], nextForm_name)(request)
#            else:
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
        key = 'orthg_'+item[:-4]
        request.session[key] = ''    
    request.session['currentForm_name'] = 'standardGeneralizedForm'
    request.session['Routines'] = str(orthg.objects.all())
    request.session['orthg_guided_answered'] = OrderedDict()
    
    ## get ready for the template
    context = {
                'formHTML': "standardGeneralizedForm",
                'form': "invalid",
                'orthg_guided_answered' : '',
                'results' : 'start'
    }
    return render_to_response('orthg/index.html', context_instance=RequestContext(request, context))

def guidedSearch(request):
#    if request.session['currentForm_name'] in form_rank:
#       form = getattr(sys.modules[__name__], request.session['currentForm_name'])(request, request.GET or None)   #handle GET and POST in the same view
#    elif request.session['currentForm_name'] in form_qrsvd:
#          form = getattr(sys.modules[__name__], request.session['currentForm_name'])(request, request.GET or None)  #handle GET and POST in the same view
#    else:
    form = getattr(sys.modules[__name__], request.session['currentForm_name'])(request.GET or None)
    if form.is_valid():
        ## get current question and user's answer
        current_question = request.session['currentForm_name'][:-4]
        formField_name = 'orthg_'+current_question
        value = form.cleaned_data[formField_name]
        choices = form.fields[formField_name].choices        
        request.session['orthg_guided_answered'].update(question_and_answer(form, value, choices))
 
        ## if user chooses to stop the search, start over; otherwise, perform the search
        if value == 'stop':
            return guidedSearch_index(request)
        else:        
            ## do search based on user's response
            lookup = "%s__contains" % current_question
            query = {lookup : value}
           #FIX!!! request.session['Routines'] = request.session['Routines'].filter(**query)
                                    
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
                        'orthg_guided_answered' : request.session['orthg_guided_answered'],
                       # 'results' : request.session['Routines']
                        }
            return render_to_response('orthg/index.html', context_instance=RequestContext(request, context))
    else:       
          return guidedSearch_index(request)

