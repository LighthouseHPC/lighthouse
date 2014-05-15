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

import datetime


######--------- Guided Search --------- ######

## help function
def question_and_answer(form, value, choices):
    for field in form:
        question = unicode(field.label)
    for choice in choices:
        if choice[0] == value:
            answer = choice[1]
    return {question: [answer]}
    

## ------------------------- show question order for different problem types -----------------------------------------------##
#svdForm_order = {
#    'symmetric':        ['complexNumber', 'matrixTypeForm', 'storageTypeForm', 'singularValuesForm', 'singularVectorsForm', 'thePrecisionForm'],
#    'Hermitian':        ['complexNumber', 'matrixTypeForm', 'storageTypeForm', 'singularValuesForm', 'singularVectorsForm', 'thePrecisionForm'],
#    'SPD':              ['complexNumber', 'matrixTypeForm', 'storageTypeForm', 'singularVectorsForm', 'thePrecisionForm'],
#    'HPD':              ['complexNumber', 'matrixTypeForm', 'storageTypeForm', 'singularVectorsForm', 'thePrecisionForm'],    
#    'upper Hessenberg': ['complexNumber', 'matrixTypeForm', 'storageTypeForm', 'singularVectorsForm', 'thePrecisionForm'],
#    'general':          ['complexNumber', 'matrixTypeForm', 'storageTypeForm', 'singularVectorsForm', 'schurForm', 'cndNumberForm', 'thePrecisionForm'],
#    }


#HessenbergForm_order = ['standardGeneralized', 'complexNumber', 'matrixType', 'storageType', 'thePrecision']
#conditionNumberForm_order = ['standardGeneralized', 'complexNumber', 'matrixType', 'thePrecision']
#balanceForm_order = ['standardGeneralized', 'complexNumber', 'storageType', 'thePrecision']
## ------------------------------------------------------------------------------------------------------------------------##



## index page
def guidedSearch_index(request):
    request.session['svd_guided_answered'] = OrderedDict()
    context = {
                'action': '/lapack_svd/problem/',
                'formHTML': "problemForm",
                'form': "invalid",
                'svd_guided_answered' : '',
                'results' : 'start'
    }
    return render_to_response('lighthouse/lapack_svd/index.html', context_instance=RequestContext(request, context))



## 'problem' question answered
@csrf_exempt
def guidedSearch_problem(request):
    form = problemForm(request.POST or None)              #handle GET and POST in the same view
    if form.is_valid(): # All validation rules pass
        request.session['svd_guided_answered'].update(question_and_answer(form, form.cleaned_data['svd_prob'], SVD_CHOICES)) #get previous question & answer
        request.session['svd_problem'] = form.cleaned_data['svd_prob']
        request.session['Routines'] = lapack_svd.objects.filter(problem=form.cleaned_data['svd_prob'])    
               
        context = {
                    'action': '/lapack_svd/standardGeneralized/',
                    'formHTML': "standardGeneralizedForm",
                    'form': "invalid",
                    'svd_guided_answered' : request.session['svd_guided_answered'],
                    'results' : request.session['Routines']
        }
    else:       
        form = problemForm() # An unbound form       
        context = {
                    'action': '/lapack_svd/problem/',
                    'formHTML': "problemForm",
                    'form': "invalid",
                    'svd_guided_answered' : '',
                    'results' : 'start'
        }
    return render_to_response('lighthouse/lapack_svd/index.html', context_instance=RequestContext(request, context))



## 'standard/generalized' question answered
@csrf_exempt
def guidedSearch_standardGeneralized(request):
    form = standardGeneralizedForm(request.POST or None)              #handle GET and POST in the same view 
    if form.is_valid(): # All validation rules pass
        request.session['svd_guided_answered'].update(question_and_answer(form, form.cleaned_data['svd_standardGeneralized'], STANDARD_CHOICES)) #get previous question & answer
        request.session['svd_standardGeneralized'] = form.cleaned_data['svd_standardGeneralized']
        request.session['Routines'] = request.session['Routines'].filter(standardGeneralized = form.cleaned_data['svd_standardGeneralized'])    
        nextForm = complexNumberForm()        
        context = {
                    'action': '/lapack_svd/complexNumber/',
                    'formHTML': "invalid",
                    'form': nextForm,
                    'svd_guided_answered' : request.session['svd_guided_answered'],
                    'results' : request.session['Routines']
        }
    else:       
        form = standardGeneralizedForm() # An unbound form       
        context = {
                    'action': '/lapack_svd/standardGeneralized/',
                    'formHTML': "invalid",
                    'form': nextForm,
                    'svd_guided_answered' : '',
                    'results' : 'start'
        }
    return render_to_response('lighthouse/lapack_svd/index.html', context_instance=RequestContext(request, context))




## 'complex number' question answered
@csrf_exempt
def guidedSearch_complexNumber(request):
    form = complexNumberForm(request.POST or None) 
    if form.is_valid():
        request.session['svd_guided_answered'].update(question_and_answer(form, form.cleaned_data['svd_complexNumber'], (('no','no'),('yes','yes'),)))    
        request.session['svd_complexNumber'] = form.cleaned_data['svd_complexNumber']

        request.session['Routines'] = request.session['Routines'].filter(complexNumber = form.cleaned_data['svd_complexNumber'])
            
        if request.session['svd_problem'] == 'balance':
            nextForm = storageTypeForm(request)
            action = '/lapack_svd/storageType/'
        else:
            nextForm = matrixTypeForm(request)
            action = '/lapack_svd/matrixType/'
        context = {
                    'action': action,
                    'formHTML': "invalid",
                    'form': nextForm,
                    'svd_guided_answered' : request.session['svd_guided_answered'],
                    'results' : request.session['Routines']
        }
    else:
        form = complexNumberForm()    
        context = {
                    'action': '/lapack_svd/complexNumber/',
                    'formHTML': "invalid",
                    'form': form,
                    'svd_guided_answered' : request.session['svd_guided_answered'],
                    'results' : request.session['Routines']
        }
    return render_to_response('lighthouse/lapack_svd/index.html', context_instance=RequestContext(request, context))




## 'matrix type' question answered
@csrf_exempt
def guidedSearch_matrixType(request):
    form = matrixTypeForm(request, request.POST or None)
    if form.is_valid():
        request.session['svd_guided_answered'].update(question_and_answer(form, form.cleaned_data['svd_matrixType'], form.fields['svd_matrixType'].choices))
        request.session['svd_matrixType'] = form.cleaned_data['svd_matrixType']
        request.session['Routines'] = request.session['Routines'].filter(matrixType=form.cleaned_data['svd_matrixType'])
        
        if request.session['svd_problem'] == 'cndNumber_of_evtrs':
            nextForm = thePrecisionForm()
            action = '/lapack_svd/thePrecision/'
        else:
            nextForm = storageTypeForm(request)
            action = '/lapack_svd/storageType/'
        context = {
                    'action': action,
                    'formHTML': "invalid",
                    'form': nextForm,
                    'svd_guided_answered' : request.session['svd_guided_answered'],
                    'results' : request.session['Routines']
        }
    else:
        form = matrixTypeForm(request)     
        context = {
                    'action': '/lapack_svd/matrixType/',
                    'formHTML': "invalid",
                    'form': form,
                    'svd_guided_answered' : request.session['svd_guided_answered'],
                    'results' : request.session['Routines']
        }
    return render_to_response('lighthouse/lapack_svd/index.html', context_instance=RequestContext(request, context))




## 'storage type' question answered
@csrf_exempt
def guidedSearch_storageType(request):
    form = storageTypeForm(request, request.POST or None)
    if form.is_valid():
        request.session['svd_guided_answered'].update(question_and_answer(form, form.cleaned_data['svd_storageType'], form.fields['svd_storageType'].choices))
        request.session['svd_storageType'] = form.cleaned_data['svd_storageType']
        request.session['Routines'] = request.session['Routines'].filter(storageType__icontains=form.cleaned_data['svd_storageType'])    
        
        if request.session['svd_problem'] in ['Hessenberg', 'balance']:
            nextForm = thePrecisionForm()
            action = '/lapack_svd/thePrecision/'
            
        else:
            if request.session['svd_matrixType'] in ['symmetric', 'Hermitian']:
                nextForm = singularValuesForm()
                action = '/lapack_svd/singularValues/'
            else:
                nextForm = singularVectorsForm()
                action = '/lapack_svd/singularVectors/'            
        context = {
                    'action': action,
                    'formHTML': "invalid",
                    'form': nextForm,
                    'svd_guided_answered' : request.session['svd_guided_answered'],
                    'results' : request.session['Routines']
        }
    else:
        form = storageTypeForm(request)       
        context = {
                    'action': '/lapack_svd/storageType/',
                    'formHTML': "invalid",
                    'form': form,
                    'svd_guided_answered' : request.session['svd_guided_answered'],
                    'results' : request.session['Routines']
        }
    return render_to_response('lighthouse/lapack_svd/index.html', context_instance=RequestContext(request, context))




## 'selected svdvalues' question answered 
@csrf_exempt
def guidedSearch_singularValues(request):
    form = singularValuesForm(request.POST or None)
    if form.is_valid():
        request.session['svd_guided_answered'].update(question_and_answer(form, form.cleaned_data['svd_singularValues'], form.fields['svd_singularValues'].choices))
        request.session['svd_singularValuesForm'] = form.cleaned_data['svd_singularValues']
        request.session['Routines'] = request.session['Routines'].filter(singularValues=form.cleaned_data['svd_singularValues'])   
        nextForm = singularVectorsForm()
        context = {
                    'action': '/lapack_svd/singularVectors/',
                    'formHTML': "invalid",
                    'form': nextForm,
                    'svd_guided_answered' : request.session['svd_guided_answered'],
                    'results' : request.session['Routines']
        }
    else:
        form = singularValuesForm()      
        context = {
                    'action': '/lapack_svd/singularValuesForm/',
                    'formHTML': "invalid",
                    'form': form,
                    'svd_guided_answered' : request.session['svd_guided_answered'],
                    'results' : request.session['Routines']
        }
    return render_to_response('lighthouse/lapack_svd/index.html', context_instance=RequestContext(request, context))




## 'singularVectorss' question answered
@csrf_exempt
def guidedSearch_singularVectors(request):
    form = singularVectorsForm(request.POST or None) 
    if form.is_valid():
        request.session['svd_guided_answered'].update(question_and_answer(form, form.cleaned_data['svd_singularVectors'], form.fields['svd_singularVectors'].choices))
        request.session['svd_singularVectorsForm'] = form.cleaned_data['svd_singularVectors']
        request.session['Routines'] = request.session['Routines'].filter(singularVectors__icontains=form.cleaned_data['svd_singularVectors'])
        
        if request.session['svd_matrixType'] == 'general' and request.session['svd_singularVectorsForm'] == 'no':
            nextForm = schurForm()
            action = '/lapack_svd/schur/'
        elif request.session['svd_matrixType'] == 'general' and request.session['svd_singularVectorsForm'] == 'yes':
            nextForm = cndNumberForm()
            action = '/lapack_svd/cndNumber/'            
        else:        
            nextForm = thePrecisionForm()
            action = '/lapack_svd/thePrecision/'            
        context = {
                    'action': action,
                    'formHTML': "invalid",
                    'form': nextForm,
                    'svd_guided_answered' : request.session['svd_guided_answered'],
                    'results' : request.session['Routines']
        }
    else:
        form = singularVectorsForm()    
        context = {
                    'action': '/lapack_svd/singularVectors/',
                    'formHTML': "invalid",
                    'form': form,
                    'svd_guided_answered' : request.session['svd_guided_answered'],
                    'results' : request.session['Routines']
        }
    return render_to_response('lighthouse/lapack_svd/index.html', context_instance=RequestContext(request, context))




## 'Schur form/vectors' question answered
@csrf_exempt
def guidedSearch_schur(request):
    form = schurForm(request.POST or None)
    if form.is_valid():
        request.session['svd_guided_answered'].update(question_and_answer(form, form.cleaned_data['svd_schur'], form.fields['svd_schur'].choices))
        request.session['svd_schurForm'] = form.cleaned_data['svd_schur']
        request.session['Routines'] = request.session['Routines'].filter(schur=form.cleaned_data['svd_schur'])       
        nextForm = cndNumberForm()           
        context = {
                    'action': '/lapack_svd/cndNumber/',
                    'formHTML': "invalid",
                    'form': nextForm,
                    'svd_guided_answered' : request.session['svd_guided_answered'],
                    'results' : request.session['Routines']
        }
    else:
        form = schurForm()        
        context = {
                    'action': '/lapack_svd/schur/',
                    'formHTML': "invalid",
                    'form': form,
                    'svd_guided_answered' : request.session['svd_guided_answered'],
                    'results' : request.session['Routines']
        }
    return render_to_response('lighthouse/lapack_svd/index.html', context_instance=RequestContext(request, context))




## 'condition number' question answered
@csrf_exempt
def guidedSearch_cndNumber(request):
    form = cndNumberForm(request.POST or None)
    if form.is_valid():
        request.session['svd_guided_answered'].update(question_and_answer(form, form.cleaned_data['svd_cndNumber'], form.fields['svd_cndNumber'].choices))
        request.session['svd_cndNumberForm'] = form.cleaned_data['svd_cndNumber']
        request.session['Routines'] = request.session['Routines'].filter(cndNumber=form.cleaned_data['svd_cndNumber'])        
        nextForm = thePrecisionForm()        
        context = {
                    'action': '/lapack_svd/thePrecision/',
                    'formHTML': "invalid",
                    'form': nextForm,
                    'svd_guided_answered' : request.session['svd_guided_answered'],
                    'results' : request.session['Routines']
        }
    else:
        form = cndNumberForm()       
        context = {
                    'action': '/lapack_svd/cndNumber/',
                    'formHTML': "invalid",
                    'form': form,
                    'svd_guided_answered' : request.session['svd_guided_answered'],
                    'results' : request.session['Routines']
        }
    return render_to_response('lighthouse/lapack_svd/index.html', context_instance=RequestContext(request, context))




## 'precision' question answered
@csrf_exempt
def guidedSearch_thePrecision(request):
    form = thePrecisionForm(request.POST or None) 
    if form.is_valid():
        request.session['svd_guided_answered'].update(question_and_answer(form, form.cleaned_data['svd_thePrecision'], form.fields['svd_thePrecision'].choices))
        request.session['svd_thePrecisionForm'] = form.cleaned_data['svd_thePrecision']
        
        if request.session['svd_complexNumber'] == 'no' and request.session['svd_thePrecisionForm'] == 'single':
            request.session['Routines'] = request.session['Routines'].filter(thePrecision='s')
        elif request.session['svd_complexNumber'] == 'no' and request.session['svd_thePrecisionForm'] == 'double':
            request.session['Routines'] = request.session['Routines'].filter(thePrecision='d')
        elif request.session['svd_complexNumber'] == 'yes' and request.session['svd_thePrecisionForm'] == 'single':
            request.session['Routines'] = request.session['Routines'].filter(thePrecision='c')
        else:
            request.session['Routines'] = request.session['Routines'].filter(thePrecision='z')
            
        context = {                             ### not pass 'action' to end the form
                    'formHTML': "invalid",
                    'svd_guided_answered' : request.session['svd_guided_answered'],
                    'results' : request.session['Routines']
        }
    else:
        form = thePrecisionForm()
        context = {
                    'action': '/lapack_svd/thePrecision/',
                    'formHTML': "invalid",
                    'form': form,
                    'svd_guided_answered' : request.session['svd_guided_answered'],
                    'results' : request.session['Routines']
        }
    return render_to_response('lighthouse/lapack_svd/index.html', context_instance=RequestContext(request, context))