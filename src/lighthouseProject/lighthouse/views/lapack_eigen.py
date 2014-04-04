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
from lighthouse.models.lapack_eigen import *

import datetime
import json


###--- Guided Search ---###
def eigen_guidedSearch(request):
    ## handle request.session setup
    if 'eigen_current_tab' not in request.session:
        request.session['eigen_current_tab'] = 'guided'
        
    if 'eigen_guided_current_form' not in request.session:
        request.session['eigen_guided_current_form'] = 'start'
        
    if 'eigen_guided_answered' not in request.session:
        request.session['eigen_guided_answered'] = {}
        
    if 'eigen_selectedRoutines' not in request.session:
        request.session['eigen_selectedRoutines'] = []
    selectedRoutines = request.session['eigen_selectedRoutines']

    selectedRoutineNames = ()
    for item in request.session['eigen_selectedRoutines']:
        if item['checkState'] == 'checked':
            selectedRoutineNames = selectedRoutineNames + (item['thePrecision']+item['routineName'],)

    context = {
#        'selectedRoutines': selectedRoutines, 
        'selectedRoutineNames' : selectedRoutineNames,
        'content_eigen_keywordSearch' : ''

    }
    

    if request.method == 'POST':        # If the form has been submitted...
        print request.POST
        #if "guided" in request.POST:
        #    request.session['eigen_current_tab'] = 'guided'
        #    if request.POST.get('guided') == 'clear':       ## if the page was NOT a submission, clear AnsweredQuestions
        #        clearAnsweredQuestions(request)             
        #    else:
        #        if 'eigen_guided_current_form' in request.session:
        #            formname = request.session['eigen_guided_current_form']
        #        else:
        #            formname = 'start'
        #        if 'eigen_guided_answered' in request.session:
        #            answered_temp = request.session['eigen_guided_answered']
        #        else:
        #            answered_temp = OrderedDict()
        #    
        #        # if it was submitted and isn't a clear request
        #        if request.method == 'POST':
        #            if request.POST.get('guided') != 'clear':  
        #                if formname not in ('start', 'finish'):
        #                    # if it's not the first or last page, check the form
        #                    # uses GuidedForm instead of FilteredForm for performance
        #                    form = GuidedForm(formname,request.POST)
        #                    if form.is_valid():
        #                         answered_temp.update({formname : (form.cleaned_data[formname],)})
        #        
        #        # if we are at the beginning, reset everything
        #        elif formname == 'start':            
        #            clearAnsweredQuestions(request)
        #            answered_temp = OrderedDict()
        #    
        #        # find the results and set the context variable
        #        results = getFilteredList(answered_temp)
        #        context = {
        #                'results' : results,
        #        }
        #        
        #        # find the page that should be shown next
        #        nextform = findNextForm(results,answered_temp)
        #    
        #        # update session variables
        #        request.session['eigen_guided_current_form'] = nextform   
        #        request.session['eigen_guided_answered'] = answered_temp
        #        
        #        answered = findAnsweredQuestions(answered_temp)
        #        
        #        if nextform != 'finish':
        #            # build a list of answered questions and update the context
        #    
        #            context.update({'eigen_guided_answered' : answered,
        #                            'content_eigen_guided_form':'lighthouse/lapack_eigen/guided/questions.html', 
        #                            'guided_form' : FilteredForm(nextform,results),
        #                            })
        #        else:
        #            context.update({'eigen_guided_answered' : answered,
        #                            'content_eigen_guided_form':'lighthouse/lapack_eigen/guided/finished.html', 
        #                            })
        #        return render_to_response(
        #            'lighthouse/lapack_eigen/index.html',
        #            context_instance=RequestContext(request,context)
        #        )

        return HttpResponse('<h1>Page was found</h1>')
    
    else:
        form = problemForm() # An unbound form

        return render_to_response(
            'lighthouse/lapack_eigen/index.html',
            context_instance=RequestContext(request, context)
        )
   
   
   
   
 
###---  clear session variable for guided search answered questions ---###
def clearAnsweredQuestions(request):
    # for key, _ in eigen_fields.items():
    #     label = 'eigen_form_' + key
    #     if label in request.session:
    #         del request.session[label]
    if 'eigen_guided_answered' in request.session:
        del request.session['eigen_guided_answered']
    request.session['eigen_guided_current_form'] = 'start'
    
    
    
###---  clear session variable for advanced guided search ---###
def clearAdvancedAnsweredQuestions(request):
    # for key, _ in eigen_fields.items():
    #     label = 'eigen_form_' + key
    #     if label in request.session:
    #         del request.session[label]
    if 'eigen_advanced_answered' in request.session:
        del request.session['eigen_advanced_answered']
    request.session['eigen_advanced_current_form'] = 'start'





    

###---  build a dictionary of questions and answers from form data ---###
def findAnsweredQuestions(answered_questions):
    results = OrderedDict()
    for key,values in answered_questions.items():
        field_label, field_choices = eigen_fields[key]
        for value in values:
            for shortval, longval in field_choices:
                if value == shortval:
                    answers = ()
                    if field_label in results:
                        answers = results[field_label]
                    answers = answers + (longval,)
                    results.update({field_label:answers})
    return results






###---  generate the context for the advanced tab ---###
def lapack_eigen_advanced_context(request):

    # retrieve session variables
    if 'eigen_advanced_current_form' in request.session:
        formname = request.session['eigen_advanced_current_form']
    else:
        formname = 'start'
    if 'eigen_advanced_answered' in request.session:
        answered_temp = request.session['eigen_advanced_answered']
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
                    for fname in eigen_advanced_forms[formname]:
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
    request.session['eigen_advanced_current_form'] = nextform   
    request.session['eigen_advanced_answered'] = answered_temp
    
    if nextform != 'finish':
        # get a readable list of answered questions


        #update the context
        context.update({'eigen_advanced_answered' : answered,
                        'content_eigen_advanced_form':'lighthouse/lapack_eigen/advanced/questions.html', 
                        'advanced_form' : AdvancedFilteredForm(nextform,results),
                        })
    else:
        context.update({'eigen_advanced_answered' : answered,
                        'content_eigen_advanced_form':'lighthouse/lapack_eigen/advanced/finished.html', 
                        })
 
    return context




@csrf_exempt
def eigen_clear_session(request):
    if request.is_ajax():
        mode = request.POST.get('clear')
        if mode == 'unchecked':
            test = request.session['eigen_selectedRoutines']
            request.session['eigen_selectedRoutines'] = []
            for item in test:
                if item['checkState'] == 'checked':                 
                    request.session['eigen_selectedRoutines'].append(item)
        # Clear checked routines            
        elif mode == 'checked':
            test = request.session['eigen_selectedRoutines']
            request.session['eigen_selectedRoutines'] = []
            for item in test:
                if item['checkState'] == 'unchecked':                   
                    request.session['eigen_selectedRoutines'].append(item)
        if mode == 'all':
            request.session['eigen_selectedRoutines'] = []
        return HttpResponse('cleared')              
    else:
        return HttpResponse('only AJAX requests are allowed!')

@csrf_exempt
def eigen_update_session(request):
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
        for item in request.session['eigen_selectedRoutines']:
            if item['thePrecision'] == selectedRoutineList[0]['thePrecision'] and item['routineName'] == selectedRoutineList[0]['routineName']:
                match = counter # Save the index
                if selectedRoutineList[0]['checkState'] == 'checked':
                    request.session['eigen_selectedRoutines'][counter]['checkState'] = 'checked'
                if selectedRoutineList[0]['checkState'] == 'unchecked':
                    request.session['eigen_selectedRoutines'][counter]['checkState'] = 'unchecked'                            
            counter += 1

        if match == -1: # The routine does not exist in request.session['selectedRoutines'], so add it
            request.session['eigen_selectedRoutines'] = request.session['eigen_selectedRoutines'] + selectedRoutineList

        # Session was modified
        request.session.modified = True
        

        # Create a list of all checked routines 
        for item in request.session['eigen_selectedRoutines']:
            if item['checkState'] == 'checked':
                selectedRoutineNames.append(item['thePrecision']+item['routineName'] + ",")
        # Return the list
        return HttpResponse(selectedRoutineNames)
    else:
        return HttpResponse('only AJAX requests are allowed!')





###--- Advanced Search ---###
def eigen_advancedSearch(request):
    if 'eigen_selectedRoutines' not in request.session:
        request.session['eigen_selectedRoutines'] = []
    selectedRoutines = request.session['eigen_selectedRoutines']

    selectedRoutineNames = ()
    for item in request.session['eigen_selectedRoutines']:
        if item['checkState'] == 'checked':
            selectedRoutineNames = selectedRoutineNames + (item['thePrecision']+item['routineName'],)

    if 'eigen_guided_answered' not in request.session:
        request.session['eigen_guided_answered'] = {}

    if 'eigen_advanced_answered' not in request.session:
        request.session['eigen_advanced_answered'] = {}

    if 'eigen_guided_current_form' not in request.session:
        request.session['eigen_guided_current_form'] = 'start'

    if 'eigen_advanced_current_form' not in request.session:
        request.session['eigen_advanced_current_form'] = 'start'

    if 'eigen_current_tab' not in request.session:
        request.session['eigen_current_tab'] = 'guided'

    context = {
#        'selectedRoutines': selectedRoutines, 
        'selectedRoutineNames' : selectedRoutineNames,
        'content_eigen_keywordSearch' : ''

    }

    current_tab = ''


    ## if the page was NOT a submission, clear AnsweredQuestions
    if request.method == 'POST':
        if "guided" in request.POST:
            request.session['eigen_current_tab'] = 'guided'
            if request.POST.get('guided') == 'clear':
                clearAnsweredQuestions(request)             
        elif "advanced" in request.POST:
            if request.POST.get('advanced') == 'clear':
                clearAdvancedAnsweredQuestions(request)
            request.session['eigen_current_tab'] = 'advanced'
        elif "keyword" in request.POST:
            request.session['eigen_current_tab'] = 'keyword'            



    ## render the page with the current tab active
    current_tab = request.session['eigen_current_tab']
    if current_tab == 'guided':
        clearAdvancedAnsweredQuestions(request)
        context.update(lapack_eigen_guided_context(request))
        context.update(lapack_eigen_advanced_context(request))

        return render_to_response(
            'lighthouse/lapack_eigen/index.html',
            context_instance=RequestContext(request,context)
        )
    elif current_tab == 'advanced':
        clearAnsweredQuestions(request)
        context.update(lapack_eigen_guided_context(request))
        context.update(lapack_eigen_advanced_context(request))
        return render_to_response(
            'lighthouse/lapack_eigen/index.html',
            {'AdvancedTab': True}, 
            context_instance=RequestContext(request,context)
        )
    elif current_tab == 'keyword':
        clearAnsweredQuestions(request)
        context.update(lapack_eigen_guided_context(request))
        clearAdvancedAnsweredQuestions(request)
        context.update(lapack_eigen_advanced_context(request))


        return render_to_response(
            'lighthouse/lapack_eigen/index.html',
            {'KeywordTab': True},
            context_instance=RequestContext(request,context)
        )