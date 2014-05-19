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
from lighthouse.models.lapack_eigen import lapack_eigen
from lighthouse.models.lapack_choiceDict import *

import datetime


######--------- Guided Search --------- ######

## help functions
def question_and_answer(form, value, choices):
    for field in form:
        question = unicode(field.label)
    for choice in choices:
        if choice[0] == value:
            answer = choice[1]
    return {question: [answer]}
    



def find_next_form(current_form, request):
    form_order = ('problemForm', 'standardGeneralizedForm', 'complexNumberForm', 'matrixTypeForm', 'storageTypeForm',
                  'selectedEVForm', 'eigenvectorForm', 'schurForm', 'cndNumberForm', 'singleDoubleForm')
    
    current_index = form_order.index(current_form)
    next_index = next(i for i in range(current_index+1, len(form_order)) if request.session['Routines'].filter(**{form_order[i][:-4]: 'none'}).count() != 0)
    next_form = getattr(sys.modules[__name__], form_order[next_index])()
    print next_form
    
    

        
    
    
## ------------------------- show question order for different problem types -----------------------------------------------##
#eigenForm_order = {
#    'symmetric':        ['standardGeneralized', 'complexNumber', 'matrixTypeForm', 'storageTypeForm', 'selectedEVForm', 'eigenvectorForm', 'singleDoubleForm'],
#    'Hermitian':        ['standardGeneralized', 'complexNumber', 'matrixTypeForm', 'storageTypeForm', 'selectedEVForm', 'eigenvectorForm', 'singleDoubleForm'],
#    'SPD':              ['standardGeneralized', 'complexNumber', 'matrixTypeForm', 'storageTypeForm', 'eigenvectorForm', 'singleDoubleForm'],
#    'HPD':              ['standardGeneralized', 'complexNumber', 'matrixTypeForm', 'storageTypeForm', 'eigenvectorForm', 'singleDoubleForm'],    
#    'upper Hessenberg': ['standardGeneralized', 'complexNumber', 'matrixTypeForm', 'storageTypeForm', 'eigenvectorForm', 'singleDoubleForm'],
#    'general':          ['standardGeneralized', 'complexNumber', 'matrixTypeForm', 'storageTypeForm', 'eigenvectorForm', 'schurForm', 'cndNumberForm', 'singleDoubleForm'],
#    }


#HessenbergForm_order = ['standardGeneralized', 'complexNumber', 'matrixType', 'storageType', 'singleDouble']
#generalized_to_standard_order = ['complexNumber', 'matrixType', 'storageType', 'singleDouble']
#conditionNumberForm_order = ['standardGeneralized', 'complexNumber', 'matrixType', 'singleDouble']
#balanceForm_order = ['standardGeneralized', 'complexNumber', 'storageType', 'singleDouble']
## ------------------------------------------------------------------------------------------------------------------------##



## index page
def guidedSearch_index(request):
    request.session['eigen_guided_answered'] = OrderedDict()
    context = {
                'action': '/lapack_eigen/problem/',
                'formHTML': "problemForm",
                'form': "invalid",
                'eigen_guided_answered' : '',
                'results' : 'start'
    }
    return render_to_response('lighthouse/lapack_eigen/index.html', context_instance=RequestContext(request, context))



## 'problem' question answered
@csrf_exempt
def guidedSearch_problem(request):
    form = problemForm(request.POST or None)              #handle GET and POST in the same view
    if form.is_valid(): # All validation rules pass
        request.session['eigen_guided_answered'].update(question_and_answer(form, form.cleaned_data['eigen_prob'], EIGENPROBLEM_CHOICES)) #get previous question & answer
        request.session['eigen_problem'] = form.cleaned_data['eigen_prob']
        request.session['Routines'] = lapack_eigen.objects.filter(problem=form.cleaned_data['eigen_prob'])
         
        find_next_form(type(form).__name__, request)
        
        if request.session['eigen_problem'] == 'generalized_to_standard':
            nextForm = complexNumberForm()
            action = '/lapack_eigen/complexNumber/'
            formHTML = "invalid"
            form = nextForm
        else:
            nextForm = standardGeneralizedForm()
            action = '/lapack_eigen/standardGeneralized/'
            formHTML = 'standardGeneralizedForm'
            form = "invalid"
               
        context = {
                    'action': action,
                    'formHTML': formHTML,
                    'form': form,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    else:       
        form = problemForm() # An unbound form       
        context = {
                    'action': '/lapack_eigen/problem/',
                    'formHTML': "problemForm",
                    'form': "invalid",
                    'eigen_guided_answered' : '',
                    'results' : 'start'
        }
    return render_to_response('lighthouse/lapack_eigen/index.html', context_instance=RequestContext(request, context))



## 'standard/generalized' question answered
@csrf_exempt
def guidedSearch_standardGeneralized(request):
    form = standardGeneralizedForm(request.POST or None)              #handle GET and POST in the same view 
    if form.is_valid(): # All validation rules pass
        request.session['eigen_guided_answered'].update(question_and_answer(form, form.cleaned_data['eigen_standardGeneralized'], STANDARD_CHOICES)) #get previous question & answer
        request.session['eigen_standardGeneralized'] = form.cleaned_data['eigen_standardGeneralized']
        request.session['Routines'] = request.session['Routines'].filter(standardGeneralized = form.cleaned_data['eigen_standardGeneralized'])    
        nextForm = complexNumberForm()        
        context = {
                    'action': '/lapack_eigen/complexNumber/',
                    'formHTML': "invalid",
                    'form': nextForm,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    else:       
        form = standardGeneralizedForm() # An unbound form       
        context = {
                    'action': '/lapack_eigen/standardGeneralized/',
                    'formHTML': "invalid",
                    'form': nextForm,
                    'eigen_guided_answered' : '',
                    'results' : 'start'
        }
    return render_to_response('lighthouse/lapack_eigen/index.html', context_instance=RequestContext(request, context))




## 'complex number' question answered
@csrf_exempt
def guidedSearch_complexNumber(request):
    form = complexNumberForm(request.POST or None) 
    if form.is_valid():
        request.session['eigen_guided_answered'].update(question_and_answer(form, form.cleaned_data['eigen_complexNumber'], (('no','no'),('yes','yes'),)))    
        request.session['eigen_complexNumber'] = form.cleaned_data['eigen_complexNumber']

        request.session['Routines'] = request.session['Routines'].filter(complexNumber = form.cleaned_data['eigen_complexNumber'])
            
        if request.session['eigen_problem'] == 'balance':
            nextForm = storageTypeForm(request)
            action = '/lapack_eigen/storageType/'
        else:
            nextForm = matrixTypeForm(request)
            action = '/lapack_eigen/matrixType/'
        context = {
                    'action': action,
                    'formHTML': "invalid",
                    'form': nextForm,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    else:
        form = complexNumberForm()    
        context = {
                    'action': '/lapack_eigen/complexNumber/',
                    'formHTML': "invalid",
                    'form': form,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    return render_to_response('lighthouse/lapack_eigen/index.html', context_instance=RequestContext(request, context))




## 'matrix type' question answered
@csrf_exempt
def guidedSearch_matrixType(request):
    form = matrixTypeForm(request, request.POST or None)
    if form.is_valid():
        request.session['eigen_guided_answered'].update(question_and_answer(form, form.cleaned_data['eigen_matrixType'], form.fields['eigen_matrixType'].choices))
        request.session['eigen_matrixType'] = form.cleaned_data['eigen_matrixType']
        request.session['Routines'] = request.session['Routines'].filter(matrixType=form.cleaned_data['eigen_matrixType'])
        
        if request.session['eigen_problem'] == 'cndNumber_of_evtrs':
            nextForm = singleDoubleForm()
            action = '/lapack_eigen/singleDouble/'
        else:
            nextForm = storageTypeForm(request)
            action = '/lapack_eigen/storageType/'
        context = {
                    'action': action,
                    'formHTML': "invalid",
                    'form': nextForm,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    else:
        form = matrixTypeForm(request)     
        context = {
                    'action': '/lapack_eigen/matrixType/',
                    'formHTML': "invalid",
                    'form': form,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    return render_to_response('lighthouse/lapack_eigen/index.html', context_instance=RequestContext(request, context))




## 'storage type' question answered
@csrf_exempt
def guidedSearch_storageType(request):
    form = storageTypeForm(request, request.POST or None)
    if form.is_valid():
        request.session['eigen_guided_answered'].update(question_and_answer(form, form.cleaned_data['eigen_storageType'], form.fields['eigen_storageType'].choices))
        request.session['eigen_storageType'] = form.cleaned_data['eigen_storageType']
        request.session['Routines'] = request.session['Routines'].filter(storageType__icontains=form.cleaned_data['eigen_storageType'])    
        
        if request.session['eigen_problem'] in ['Hessenberg', 'generalized_to_standard', 'balance']:
            nextForm = singleDoubleForm()
            action = '/lapack_eigen/singleDouble/'
            
        else:
            if request.session['eigen_matrixType'] in ['symmetric', 'Hermitian']:
                nextForm = selectedEVForm()
                action = '/lapack_eigen/selectedEV/'
            else:
                nextForm = eigenvectorForm()
                action = '/lapack_eigen/eigenvector/'            
        context = {
                    'action': action,
                    'formHTML': "invalid",
                    'form': nextForm,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    else:
        form = storageTypeForm(request)       
        context = {
                    'action': '/lapack_eigen/storageType/',
                    'formHTML': "invalid",
                    'form': form,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    return render_to_response('lighthouse/lapack_eigen/index.html', context_instance=RequestContext(request, context))




## 'selected eigenvalues' question answered 
@csrf_exempt
def guidedSearch_selectedEV(request):
    form = selectedEVForm(request.POST or None)
    if form.is_valid():
        request.session['eigen_guided_answered'].update(question_and_answer(form, form.cleaned_data['eigen_selectedEV'], form.fields['eigen_selectedEV'].choices))
        request.session['eigen_selectedEVForm'] = form.cleaned_data['eigen_selectedEV']
        request.session['Routines'] = request.session['Routines'].filter(selectedEV=form.cleaned_data['eigen_selectedEV'])   
        nextForm = eigenvectorForm()
        context = {
                    'action': '/lapack_eigen/eigenvector/',
                    'formHTML': "invalid",
                    'form': nextForm,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    else:
        form = selectedEVForm()      
        context = {
                    'action': '/lapack_eigen/selectedEVForm/',
                    'formHTML': "invalid",
                    'form': form,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    return render_to_response('lighthouse/lapack_eigen/index.html', context_instance=RequestContext(request, context))




## 'eigenvectors' question answered
@csrf_exempt
def guidedSearch_eigenvector(request):
    form = eigenvectorForm(request.POST or None) 
    if form.is_valid():
        request.session['eigen_guided_answered'].update(question_and_answer(form, form.cleaned_data['eigen_eigenvector'], form.fields['eigen_eigenvector'].choices))
        request.session['eigen_eigenvectorForm'] = form.cleaned_data['eigen_eigenvector']
        request.session['Routines'] = request.session['Routines'].filter(eigenvector__icontains=form.cleaned_data['eigen_eigenvector'])
        
        if request.session['eigen_matrixType'] == 'general' and request.session['eigen_eigenvectorForm'] == 'no':
            nextForm = schurForm()
            action = '/lapack_eigen/schur/'
        elif request.session['eigen_matrixType'] == 'general' and request.session['eigen_eigenvectorForm'] == 'yes':
            nextForm = cndNumberForm()
            action = '/lapack_eigen/cndNumber/'            
        else:        
            nextForm = singleDoubleForm()
            action = '/lapack_eigen/singleDouble/'            
        context = {
                    'action': action,
                    'formHTML': "invalid",
                    'form': nextForm,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    else:
        form = eigenvectorForm()    
        context = {
                    'action': '/lapack_eigen/eigenvector/',
                    'formHTML': "invalid",
                    'form': form,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    return render_to_response('lighthouse/lapack_eigen/index.html', context_instance=RequestContext(request, context))




## 'Schur form/vectors' question answered
@csrf_exempt
def guidedSearch_schur(request):
    form = schurForm(request.POST or None)
    if form.is_valid():
        request.session['eigen_guided_answered'].update(question_and_answer(form, form.cleaned_data['eigen_schur'], form.fields['eigen_schur'].choices))
        request.session['eigen_schurForm'] = form.cleaned_data['eigen_schur']
        request.session['Routines'] = request.session['Routines'].filter(schur=form.cleaned_data['eigen_schur'])       
        nextForm = cndNumberForm()           
        context = {
                    'action': '/lapack_eigen/cndNumber/',
                    'formHTML': "invalid",
                    'form': nextForm,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    else:
        form = schurForm()        
        context = {
                    'action': '/lapack_eigen/schur/',
                    'formHTML': "invalid",
                    'form': form,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    return render_to_response('lighthouse/lapack_eigen/index.html', context_instance=RequestContext(request, context))




## 'condition number' question answered
@csrf_exempt
def guidedSearch_cndNumber(request):
    form = cndNumberForm(request.POST or None)
    if form.is_valid():
        request.session['eigen_guided_answered'].update(question_and_answer(form, form.cleaned_data['eigen_cndNumber'], form.fields['eigen_cndNumber'].choices))
        request.session['eigen_cndNumberForm'] = form.cleaned_data['eigen_cndNumber']
        request.session['Routines'] = request.session['Routines'].filter(cndNumber=form.cleaned_data['eigen_cndNumber'])        
        nextForm = singleDoubleForm()        
        context = {
                    'action': '/lapack_eigen/singleDouble/',
                    'formHTML': "invalid",
                    'form': nextForm,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    else:
        form = cndNumberForm()       
        context = {
                    'action': '/lapack_eigen/cndNumber/',
                    'formHTML': "invalid",
                    'form': form,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    return render_to_response('lighthouse/lapack_eigen/index.html', context_instance=RequestContext(request, context))




## 'precision' question answered
@csrf_exempt
def guidedSearch_singleDouble(request):
    form = singleDoubleForm(request.POST or None) 
    if form.is_valid():
        request.session['eigen_guided_answered'].update(question_and_answer(form, form.cleaned_data['eigen_singleDouble'], form.fields['eigen_singleDouble'].choices))
        request.session['eigen_singleDoubleForm'] = form.cleaned_data['eigen_singleDouble']
        request.session['Routines'] = request.session['Routines'].filter(singleDouble=form.cleaned_data['eigen_singleDouble'])
            
        context = {                             ### not pass 'action' to end the form
                    'formHTML': "invalid",
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    else:
        form = singleDoubleForm()
        context = {
                    'action': '/lapack_eigen/singleDouble/',
                    'formHTML': "invalid",
                    'form': form,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    return render_to_response('lighthouse/lapack_eigen/index.html', context_instance=RequestContext(request, context))