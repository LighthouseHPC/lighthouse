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
#from lighthouse.models.lapack_eigen import *

import datetime



def question_and_answer(form, value, choices):
    for field in form:
        question = unicode(field.label)
    for choice in choices:
        if choice[0] == value:
            answer = choice[1]
    return {question: [answer]}
    




######--------- Guided Search --------- ######

#standardForm_order = {
#    'symmetric':        ['complexNumber', 'matrixTypeForm', 'storageTypeForm', 'selectedEVForm', 'eigenvectorForm', 'thePrecisionForm'],
#    'Hermitian':        ['complexNumber', 'matrixTypeForm', 'storageTypeForm', 'selectedEVForm', 'eigenvectorForm', 'thePrecisionForm'],
#    'SPD':              ['complexNumber', 'matrixTypeForm', 'storageTypeForm', 'eigenvectorForm', 'thePrecisionForm'],
#    'HPD':              ['complexNumber', 'matrixTypeForm', 'storageTypeForm', 'eigenvectorForm', 'thePrecisionForm'],    
#    'upper Hessenberg': ['complexNumber', 'matrixTypeForm', 'storageTypeForm', 'eigenvectorForm', 'thePrecisionForm'],
#    'general':          ['complexNumber', 'matrixTypeForm', 'storageTypeForm', 'eigenvectorForm', 'schurForm', 'cndNumberForm', 'thePrecisionForm'],
#    }




def guidedSearch_index(request):
    request.session['eigen_guided_answered'] = OrderedDict()
    request.session['eigen_complexNumber'] = ''
    context = {
                'action': '/lapack_eigen/problem/',
                'formHTML': "problemForm",
                'form': "invalid",
                'eigen_guided_answered' : '',
                'results' : 'start'
    }
    return render_to_response(
        'lighthouse/lapack_eigen/index.html',
        context_instance=RequestContext(request, context)
    )




@csrf_exempt
def guidedSearch_problem(request):
    form = problemForm(request.POST or None)              #handle GET and POST in the same view 
    if form.is_valid(): # All validation rules pass
        request.session['eigen_guided_answered'].update(question_and_answer(form, form.cleaned_data['eigen_prob'], Problem_choices)) #get previous question & answer
        modelName = 'lapack_eigen_'+form.cleaned_data['eigen_prob']
        request.session['Routines'] = get_model('lighthouse',modelName).objects.all()    
        nextForm = complexNumberForm()

        context = {
                    'action': '/lapack_eigen/complexNumber/',
                    'formHTML': "invalid",
                    'form': nextForm,
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
    return render_to_response(
        'lighthouse/lapack_eigen/index.html',
        context_instance=RequestContext(request, context)
    )





@csrf_exempt
def guidedSearch_complexNumber(request):
    form = complexNumberForm(request.POST or None) 
    if form.is_valid():
        request.session['eigen_guided_answered'].update(question_and_answer(form, form.cleaned_data['eigen_complexNumber'], (('no','No'),('yes','Yes'),)))    
        request.session['eigen_complexNumber'] = form.cleaned_data['eigen_complexNumber']
        request.session['Routines'] = request.session['Routines'].filter(complexNumber=form.cleaned_data['eigen_complexNumber'])
        nextForm = matrixTypeForm(request)
        
        context = {
                    'action': '/lapack_eigen/matrixType/',
                    'formHTML': "invalid",
                    'form': nextForm,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    else:
        form = complexNumberForm() # An unbound form       
        context = {
                    'action': '/lapack_eigen/complexNumber/',
                    'formHTML': "invalid",
                    'form': form,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    return render_to_response(
        'lighthouse/lapack_eigen/index.html',
        context_instance=RequestContext(request, context)
    )





@csrf_exempt
def guidedSearch_matrixType(request):
    form = matrixTypeForm(request, request.POST or None)
    if form.is_valid():
        request.session['eigen_guided_answered'].update(question_and_answer(form, form.cleaned_data['eigen_matrixType'], form.fields['eigen_matrixType'].choices))
        request.session['eigen_matrixType'] = form.cleaned_data['eigen_matrixType']
        request.session['Routines'] = request.session['Routines'].filter(matrixType=form.cleaned_data['eigen_matrixType'])       
        nextForm = storageTypeForm(request)
       
        context = {
                    'action': '/lapack_eigen/storageType/',
                    'formHTML': "invalid",
                    'form': nextForm,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    else:
        form = matrixTypeForm(request) # An unbound form       
        context = {
                    'action': '/lapack_eigen/matrixType/',
                    'formHTML': "invalid",
                    'form': form,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    return render_to_response(
        'lighthouse/lapack_eigen/index.html',
        context_instance=RequestContext(request, context)
    )





@csrf_exempt
def guidedSearch_storageType(request):
    form = storageTypeForm(request, request.POST or None)
    if form.is_valid():
        request.session['eigen_guided_answered'].update(question_and_answer(form, form.cleaned_data['eigen_storageType'], form.fields['eigen_storageType'].choices))
        request.session['eigen_storageType'] = form.cleaned_data['eigen_storageType']
        request.session['Routines'] = request.session['Routines'].filter(storageType=form.cleaned_data['eigen_storageType'])
        
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
        form = storageTypeForm(request) # An unbound form       
        context = {
                    'action': '/lapack_eigen/storageType/',
                    'formHTML': "invalid",
                    'form': form,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    return render_to_response(
        'lighthouse/lapack_eigen/index.html',
        context_instance=RequestContext(request, context)
    )




@csrf_exempt
def guidedSearch_selectedEV(request):
    form = selectedEVForm(request.POST or None)
    if form.is_valid():
        request.session['eigen_guided_answered'].update(question_and_answer(form, form.cleaned_data['eigen_selectedEV'], form.fields['eigen_selectedEV'].choices))
        request.session['eigen_selectedEVForm'] = form.cleaned_data['eigen_selectedEV']
        request.session['Routines'] = request.session['Routines'].filter(selectedEV=form.cleaned_data['eigen_selectedEV'])
        
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
        form = selectedEVFormForm() # An unbound form       
        context = {
                    'action': '/lapack_eigen/selectedEVForm/',
                    'formHTML': "invalid",
                    'form': form,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    return render_to_response(
        'lighthouse/lapack_eigen/index.html',
        context_instance=RequestContext(request, context)
    )



@csrf_exempt
def guidedSearch_eigenvector(request):
    form = eigenvectorForm(request.POST or None) 
    if form.is_valid():
        request.session['eigen_guided_answered'].update(question_and_answer(form, form.cleaned_data['eigen_eigenvector'], form.fields['eigen_eigenvector'].choices))
        request.session['eigen_eigenvectorForm'] = form.cleaned_data['eigen_eigenvector']
        request.session['Routines'] = request.session['Routines'].filter(eigenvector=form.cleaned_data['eigen_eigenvector'])
        
        if request.session['eigen_matrixType'] == 'general' and request.session['eigen_eigenvectorForm'] == 'no':
            nextForm = schurForm()
            action = '/lapack_eigen/schur/'
        elif request.session['eigen_matrixType'] == 'general' and request.session['eigen_eigenvectorForm'] == 'yes':
            nextForm = cndNumberForm()
            action = '/lapack_eigen/cndNumber/'            
        else:        
            nextForm = thePrecisionForm()
            action = '/lapack_eigen/thePrecision/'
            
        context = {
                    'action': action,
                    'formHTML': "invalid",
                    'form': nextForm,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    else:
        form = eigenvectorForm() # An unbound form       
        context = {
                    'action': '/lapack_eigen/eigenvector/',
                    'formHTML': "invalid",
                    'form': form,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    return render_to_response(
        'lighthouse/lapack_eigen/index.html',
        context_instance=RequestContext(request, context)
    )



@csrf_exempt
def guidedSearch_schur(request):
    form = schurForm(request.POST or None)
    if form.is_valid():
        request.session['eigen_guided_answered'].update(question_and_answer(form, form.cleaned_data['eigen_schur'], form.fields['eigen_schur'].choices))
        request.session['eigen_schurForm'] = form.cleaned_data['eigen_schur']
        request.session['Routines'] = request.session['Routines'].filter(schur=form.cleaned_data['eigen_schur'])
        
        nextForm = cndNumberForm()
        action = '/lapack_eigen/cndNumber/'            
        context = {
                    'action': action,
                    'formHTML': "invalid",
                    'form': nextForm,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    else:
        form = schurForm() # An unbound form       
        context = {
                    'action': '/lapack_eigen/schur/',
                    'formHTML': "invalid",
                    'form': form,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    return render_to_response(
        'lighthouse/lapack_eigen/index.html',
        context_instance=RequestContext(request, context)
    )




@csrf_exempt
def guidedSearch_cndNumber(request):
    form = cndNumberForm(request.POST or None)
    if form.is_valid():
        request.session['eigen_guided_answered'].update(question_and_answer(form, form.cleaned_data['eigen_cndNumber'], form.fields['eigen_cndNumber'].choices))
        request.session['eigen_cndNumberForm'] = form.cleaned_data['eigen_cndNumber']
        request.session['Routines'] = request.session['Routines'].filter(cndNumber=form.cleaned_data['eigen_cndNumber'])
        
        nextForm = thePrecisionForm()
        action = '/lapack_eigen/thePrecision/'            
        context = {
                    'action': action,
                    'formHTML': "invalid",
                    'form': nextForm,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    else:
        form = cndNumberForm() # An unbound form       
        context = {
                    'action': '/lapack_eigen/cndNumber/',
                    'formHTML': "invalid",
                    'form': form,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    return render_to_response(
        'lighthouse/lapack_eigen/index.html',
        context_instance=RequestContext(request, context)
    )





@csrf_exempt
def guidedSearch_thePrecision(request):
    form = thePrecisionForm(request.POST or None) 
    if form.is_valid():
        request.session['eigen_guided_answered'].update(question_and_answer(form, form.cleaned_data['eigen_thePrecision'], form.fields['eigen_thePrecision'].choices))
        request.session['eigen_thePrecisionForm'] = form.cleaned_data['eigen_thePrecision']
        
        if request.session['eigen_complexNumber'] == 'no' and request.session['eigen_thePrecisionForm'] == 'single':
            request.session['Routines'] = request.session['Routines'].filter(thePrecision='s')
        elif request.session['eigen_complexNumber'] == 'no' and request.session['eigen_thePrecisionForm'] == 'double':
            request.session['Routines'] = request.session['Routines'].filter(thePrecision='d')
        elif request.session['eigen_complexNumber'] == 'yes' and request.session['eigen_thePrecisionForm'] == 'single':
            request.session['Routines'] = request.session['Routines'].filter(thePrecision='c')
        else:
            request.session['Routines'] = request.session['Routines'].filter(thePrecision='z')
                  
        context = {
                    'formHTML': "invalid",
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    else:
        form = thePrecisionForm() # An unbound form       
        context = {
                    'action': '/lapack_eigen/thePrecision/',
                    'formHTML': "invalid",
                    'form': form,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
                    'results' : request.session['Routines']
        }
    return render_to_response(
        'lighthouse/lapack_eigen/index.html',
        context_instance=RequestContext(request, context)
    )