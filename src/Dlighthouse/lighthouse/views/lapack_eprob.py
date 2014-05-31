import string, types, sys, os, StringIO, re, shlex, json, zipfile
from collections import OrderedDict
from django.contrib.auth.decorators import login_required
from django.core.servers.basehttp import FileWrapper
from django.http import HttpResponse, HttpResponseNotFound
from django.shortcuts import render_to_response, redirect, render
from django.template import RequestContext
from django.template.loader import render_to_string
from django.views.decorators.csrf import csrf_exempt
from lighthouse.forms.lapack_eprob import *
from lighthouse.models.lapack_eprob import *

import datetime
import json

# build a dictionary of questions and answers from form data
def findAnsweredQuestions(answered_questions):
    results = OrderedDict()
    for key,values in answered_questions.items():
        field_label, field_choices = eprob_fields[key]
        for value in values:
            for shortval, longval in field_choices:
                if value == shortval:
                    answers = ()
                    if field_label in results:
                        answers = results[field_label]
                    answers = answers + (longval,)
                    results.update({field_label:answers})
    return results

# clear session variable for advanced guided search
def clearAdvancedAnsweredQuestions(request):
    # for key, _ in eprob_fields.items():
    #     label = 'eprob_form_' + key
    #     if label in request.session:
    #         del request.session[label]
    if 'eprob_advanced_answered' in request.session:
        del request.session['eprob_advanced_answered']
    request.session['eprob_advanced_current_form'] = 'start'


# clear session variable for guided search answered questions
def clearAnsweredQuestions(request):
    # for key, _ in eprob_fields.items():
    #     label = 'eprob_form_' + key
    #     if label in request.session:
    #         del request.session[label]
    if 'eprob_guided_answered' in request.session:
        del request.session['eprob_guided_answered']
    request.session['eprob_guided_current_form'] = 'start'


# generate the context for the guided search tab
def lapack_eprob_guided_context(request):


    if 'eprob_guided_current_form' in request.session:
        formname = request.session['eprob_guided_current_form']
    else:
        formname = 'start'
    if 'eprob_guided_answered' in request.session:
        answered_temp = request.session['eprob_guided_answered']
    else:
        answered_temp = OrderedDict()

    # if it was submitted and isn't a clear request
    if request.method == 'POST':
        if request.POST.get('guided') != 'clear':  
            if formname not in ('start', 'finish'):
                # if it's not the first or last page, check the form
                # uses GuidedForm instead of FilteredForm for performance
                form = GuidedForm(formname,request.POST)
                if form.is_valid():
                     answered_temp.update({formname : (form.cleaned_data[formname],)})
    
    # if we are at the beginning, reset everything
    elif formname == 'start':            
        clearAnsweredQuestions(request)
        answered_temp = OrderedDict()

    # find the results and set the context variable
    results = getFilteredList(answered_temp)
    context = {
            'results' : results,
    }
    
    # find the page that should be shown next
    nextform = findNextForm(results,answered_temp)

    # update session variables
    request.session['eprob_guided_current_form'] = nextform   
    request.session['eprob_guided_answered'] = answered_temp
    
    answered = findAnsweredQuestions(answered_temp)
    
    if nextform != 'finish':
        # build a list of answered questions and update the context

        context.update({'eprob_guided_answered' : answered,
                        'content_eprob_guided_form':'lighthouse/lapack_eprob/guided/questions.html', 
                        'guided_form' : FilteredForm(nextform,results),
                        })
    else:
        context.update({'eprob_guided_answered' : answered,
                        'content_eprob_guided_form':'lighthouse/lapack_eprob/guided/finished.html', 
                        })
    # render the result to the page
    return context




# generate the context for the advanced tab
def lapack_eprob_advanced_context(request):

    # retrieve session variables
    if 'eprob_advanced_current_form' in request.session:
        formname = request.session['eprob_advanced_current_form']
    else:
        formname = 'start'
    if 'eprob_advanced_answered' in request.session:
        answered_temp = request.session['eprob_advanced_answered']
    else:
        answered_temp = OrderedDict()

    # if request was a POST request
    if request.method == 'POST':
        # and it wasn't a clear request
        if request.POST.get('advanced') != 'clear':
            # if it's not the beginning or the end of search
            if formname not in ('start','finish'):
                # generate a form for that page and check validity
                sform = AdvancedForm(formname,request.POST)
                if sform.is_valid():
                    # if it's valid, check all the fields for that page
                    for fname in eprob_advanced_forms[formname]:
                        for answer in sform.cleaned_data.get(fname):
                            # for each answer, update the list of values
                            answers = ()
                            if fname in answered_temp:
                                answers = answered_temp[fname]
                            answers = answers + (answer,)
                            answered_temp.update({fname:answers})
                # debugging - if the form ever isn't valid bad things have happened
                #else:
                #    return HttpResponse(formname + ' ' + json.dumps(sform.errors))

    # if the formname is set to start, start over
    if formname == 'start':            
        clearAdvancedAnsweredQuestions(request)
        answered_temp = OrderedDict()

    # find all possible results
    results = getFilteredList(answered_temp)
    context = {
        'results' : results,
    }

    # figure out which form is next
    nextform = findNextFormAdvanced(results,answered_temp)
 
    # get a list of answered questions
    answered = findAnsweredQuestions(answered_temp)     
    
    # update session variables
    request.session['eprob_advanced_current_form'] = nextform   
    request.session['eprob_advanced_answered'] = answered_temp
    
    if nextform != 'finish':
        # get a readable list of answered questions


        #update the context
        context.update({'eprob_advanced_answered' : answered,
                        'content_eprob_advanced_form':'lighthouse/lapack_eprob/advanced/questions.html', 
                        'advanced_form' : AdvancedFilteredForm(nextform,results),
                        })
    else:
        context.update({'eprob_advanced_answered' : answered,
                        'content_eprob_advanced_form':'lighthouse/lapack_eprob/advanced/finished.html', 
                        })
 
    return context


# handle common setup and render the page
def lapack_eprob(request):
    if 'eprob_selectedRoutines' not in request.session:
        request.session['eprob_selectedRoutines'] = []
    selectedRoutines = request.session['eprob_selectedRoutines']

    selectedRoutineNames = ()
    for item in request.session['eprob_selectedRoutines']:
        if item['checkState'] == 'checked':
            selectedRoutineNames = selectedRoutineNames + (item['thePrecision']+item['routineName'],)

    if 'eprob_guided_answered' not in request.session:
        request.session['eprob_guided_answered'] = {}

    if 'eprob_advanced_answered' not in request.session:
        request.session['eprob_advanced_answered'] = {}

    if 'eprob_guided_current_form' not in request.session:
        request.session['eprob_guided_current_form'] = 'start'

    if 'eprob_advanced_current_form' not in request.session:
        request.session['eprob_advanced_current_form'] = 'start'


    if 'eprob_current_tab' not in request.session:
        request.session['eprob_current_tab'] = 'guided'

    context = {
#        'selectedRoutines': selectedRoutines, 
        'selectedRoutineNames' : selectedRoutineNames,
        'content_eprob_keywordSearch' : ''

    }

    current_tab = ''

    # if the page was a submission, handle that accordingly
    if request.method == 'POST':
        if "advanced" in request.POST:
            #if POST was a clear request
            if request.POST.get('advanced') == 'clear':
                clearAdvancedAnsweredQuestions(request)
            request.session['eprob_current_tab'] = 'advanced'
        elif "keyword" in request.POST:
            request.session['eprob_current_tab'] = 'keyword'            
        elif "guided" in request.POST:  
            #if POST was a clear request
            if request.POST.get('guided') == 'clear':
                clearAnsweredQuestions(request)  
            request.session['eprob_current_tab'] = 'guided'


    # render the page with the current tab active
    current_tab = request.session['eprob_current_tab']
    if current_tab == 'advanced':
        # advanced search

        # clear guided search
        clearAnsweredQuestions(request)
        context.update(lapack_eprob_guided_context(request))
        context.update(lapack_eprob_advanced_context(request))
        return render_to_response(
            'lighthouse/lapack_eprob/index.html',
            {'AdvancedTab': True}, 
            context_instance=RequestContext(request,context)
        )
    elif current_tab == 'keyword':
        # advanced search

        # clear both guided and advanced search
        clearAnsweredQuestions(request)
        context.update(lapack_eprob_guided_context(request))
        clearAdvancedAnsweredQuestions(request)
        context.update(lapack_eprob_advanced_context(request))


        return render_to_response(
            'lighthouse/lapack_eprob/index.html',
            {'KeywordTab': True},
            context_instance=RequestContext(request,context)
        )
    else:
        # guided search

        # clear advanced search
        clearAdvancedAnsweredQuestions(request)

        context.update(lapack_eprob_advanced_context(request))
        context.update(lapack_eprob_guided_context(request))

        return render_to_response(
            'lighthouse/lapack_eprob/index.html',
            context_instance=RequestContext(request,context)
        )

@csrf_exempt
def eprob_clear_session(request):
    if request.is_ajax():
        mode = request.POST.get('clear')
        if mode == 'unchecked':
            test = request.session['eprob_selectedRoutines']
            request.session['eprob_selectedRoutines'] = []
            for item in test:
                if item['checkState'] == 'checked':                 
                    request.session['eprob_selectedRoutines'].append(item)
        # Clear checked routines            
        elif mode == 'checked':
            test = request.session['eprob_selectedRoutines']
            request.session['eprob_selectedRoutines'] = []
            for item in test:
                if item['checkState'] == 'unchecked':                   
                    request.session['eprob_selectedRoutines'].append(item)
        if mode == 'all':
            request.session['eprob_selectedRoutines'] = []
        return HttpResponse('cleared')              
    else:
        return HttpResponse('only AJAX requests are allowed!')

@csrf_exempt
def eprob_update_session(request):
    if request.is_ajax():

        selectedRoutineNames = []
        selectedRoutineList = [{
             "thePrecision": request.POST.get('precision'),
             "routineName": request.POST.get('routineName'),
             "matrixType": request.POST.get('matrixType'),
             "storageType": request.POST.get('storageType'),
             "id": request.POST.get('idn'),
             "url": request.POST.get('url'),
             "checkState": request.POST.get('checkState')
        }]
        
        # Check if the routine already exists in request.session['selectedRoutines'], if it does save it's index
        counter = 0
        match = -1
        for item in request.session['eprob_selectedRoutines']:
            if item['thePrecision'] == selectedRoutineList[0]['thePrecision'] and item['routineName'] == selectedRoutineList[0]['routineName']:
                match = counter # Save the index
                if selectedRoutineList[0]['checkState'] == 'checked':
                    request.session['eprob_selectedRoutines'][counter]['checkState'] = 'checked'
                if selectedRoutineList[0]['checkState'] == 'unchecked':
                    request.session['eprob_selectedRoutines'][counter]['checkState'] = 'unchecked'                      
      
            counter += 1

        if match == -1: # The routine does not exist in request.session['selectedRoutines'], so add it
            request.session['eprob_selectedRoutines'] = request.session['eprob_selectedRoutines'] + selectedRoutineList

        # Session was modified
        request.session.modified = True
        

        # Create a list of all checked routines 
        for item in request.session['eprob_selectedRoutines']:
            if item['checkState'] == 'checked':
                selectedRoutineNames.append(item['thePrecision']+item['routineName'] + ",")
        # Return the list
        return HttpResponse(selectedRoutineNames)
    else:
        return HttpResponse('only AJAX requests are allowed!')