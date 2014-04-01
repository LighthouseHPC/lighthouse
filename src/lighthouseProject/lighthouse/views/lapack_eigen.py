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
    



@csrf_exempt
def guidedSearch_problem(request):
    if request.method == 'POST': # If the form has been submitted...
        form = problemForm(request.POST) # A form bound to the POST data
        if form.is_valid(): # All validation rules pass
            if 'eigen_guided_answered' in request.session:
                request.session['eigen_guided_answered'] = OrderedDict()
            request.session['eigen_guided_answered'].update(question_and_answer(form, form.cleaned_data['eigen_prob'], Problem_choices)) #get previous question & answer
            nextForm = complexForm()
            context = {
                        'action': '/lapack_eigen/complex/',
                        'formHTML': "invalid",
                        'form': nextForm,
                        'eigen_guided_answered' : request.session['eigen_guided_answered'],
            }
        else:
            print "submission not valid"
    else:
        form = problemForm() # An unbound form       
        context = {
                    'action': '/lapack_eigen/',
                    'formHTML': "problemForm",
                    'form': "invalid",
                    'eigen_guided_answered' : '',
        }
    return render_to_response(
        'lighthouse/lapack_eigen/index.html',
        context_instance=RequestContext(request, context)
    )


@csrf_exempt
def guidedSearch_complex(request):
    if request.method == 'POST': 
        form = complexForm(request.POST) 
        if form.is_valid():
            request.session['eigen_guided_answered'].update(question_and_answer(form, form.cleaned_data['eigen_complex'], (('real','No'),('complex','Yes'),)))
            if 'eigen_complex' in request.session:
                request.session['eigen_complex'] = form.cleaned_data['eigen_complex']
            nextForm = matrixTypeForm(request)
            context = {
                        'action': '/lapack_eigen/matrixType/',
                        'formHTML': "invalid",
                        'form': nextForm,
                        'eigen_guided_answered' : request.session['eigen_guided_answered'],
            }
        else:
            print "submission not valid"
    else:
        form = complexForm() # An unbound form       
        context = {
                    'action': '/lapack_eigen/complex/',
                    'formHTML': "invalid",
                    'form': form,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
        }
    return render_to_response(
        'lighthouse/lapack_eigen/index.html',
        context_instance=RequestContext(request, context)
    )



@csrf_exempt
def guidedSearch_matrixType(request):
    if request.method == 'POST':
        form = matrixTypeForm(request, request.POST)
        if form.is_valid():
            request.session['eigen_guided_answered'].update(question_and_answer(form, form.cleaned_data['eigen_matrixType'], form.fields['eigen_matrixType'].choices))
            print request.session['eigen_guided_answered']
            if 'eigen_matrixType' in request.session:
                request.session['eigen_matrixType'] = form.cleaned_data['eigen_matrixType']
            nextForm = matrixTypeForm(request)
            context = {
                        #'action': '/lapack_eigen/matrixType/',
                        'formHTML': "invalid",
                        'form': nextForm,
                        'eigen_guided_answered' : request.session['eigen_guided_answered'],
            }
        else:
            print "submission not valid"
    else:
        form = matrixTypeForm(request) # An unbound form       
        context = {
                    'action': '/lapack_eigen/matrixType/',
                    'formHTML': "invalid",
                    'form': form,
                    'eigen_guided_answered' : request.session['eigen_guided_answered'],
        }
    return render_to_response(
        'lighthouse/lapack_eigen/index.html',
        context_instance=RequestContext(request, context)
    )    