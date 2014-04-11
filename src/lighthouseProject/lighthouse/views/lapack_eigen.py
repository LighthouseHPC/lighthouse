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
    form = problemForm(request.POST or None)                #handle GET and POST in the same view
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
    form = matrixTypeForm(request, request.POST)
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
    form = storageTypeForm(request, request.POST)
    if form.is_valid():
        request.session['eigen_guided_answered'].update(question_and_answer(form, form.cleaned_data['eigen_storageType'], form.fields['eigen_storageType'].choices))
        request.session['eigen_storageType'] = form.cleaned_data['eigen_storageType']
        request.session['Routines'] = request.session['Routines'].filter(storageType=form.cleaned_data['eigen_storageType'])
        nextForm = selectedEVForm()
        context = {
                    #'action': '/lapack_eigen/selectedEV/',
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