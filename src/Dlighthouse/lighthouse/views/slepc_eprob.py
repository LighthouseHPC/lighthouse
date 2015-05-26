# Create your views here.
import os
from django.http import HttpResponse
from django.shortcuts import render_to_response, redirect, render
from django.template import RequestContext
from django.views.decorators.csrf import csrf_exempt
from lighthouse.forms.slepc_eprob import *
from lighthouse.models.slepc_eprob import *


def slepc_eprob(request):

    if request.method == 'POST':
        form = SlepcGuidedForm(request.POST)
        data = []
        message = form.errors#request.POST#
        #type = request.POST.get('type')
        if form.is_valid():
            data = form.cleaned_data
            message = form.cleaned_data
        request.session['form'] = form
        request.session['results'] = getSelectedRoutines(data)

    
    else:
        message = 'Checking'
        request.session['form'] = SlepcGuidedForm()
        request.session['results'] = []

    request.session['selectedRoutines'] = []
    #remove later
    context = {
        'form'     : request.session['form'],
        'results'  : request.session['results'],
        #'message'  : request.session['message'],
        'notSelectedRoutines': request.session['notSelectedRoutines'],
		'selectedRoutines':request.session['selectedRoutines'] 
    }

    return render_to_response(
            'lighthouse/slepc_eprob/index.html', 
	    context_instance=RequestContext(request,context)
    )

def filterSelectedRoutines(request):	
	request.session['notSelectedRoutines'] = request.session['Routines']

	for item in request.session['selectedRoutines']:
		request.session['notSelectedRoutines'] = request.session['notSelectedRoutines'].exclude(Q(routineName=item['routineName']))	
	
	request.session.modified = True

@csrf_exempt
def update_slepc_session(request):
	if request.is_ajax():
		#alert('hi');
		selectedRoutineNames = []
		selectedRoutineList = [{
			"routine": request.POST.get('routineName'),
		}]

		if selectedRoutineList[0] not in request.session['selectedRoutines']:
			request.session['selectedRoutines'] = request.session['selectedRoutines'] + selectedRoutineList

		return HttpResponse('Dropped '+request.POST.get('routineName'))
	else:
		return HttpResponse('only AJAX requests are allowed!')
		

def generateTemplate(request):
	method=request.session['method'] # get the method from request 
	form = request.session['form']
	if form.is_valid():
            data = form.cleaned_data
	    script = getScript(data) + ' -eps_type ' + method
	    
	code = getCode();
	
	context = {
	  		'form': request.session['form'],
			'results'  : request.session['results'],
			'selectedRoutines':request.session['selectedRoutines'],
	  		#'message': request.session['message'], # debug purpose use
			'code': code,
			'script':script, # Have not added makefile check if needed
		  }
	return render_to_response(
            'lighthouse/slepc_eprob/index.html', 
	    context_instance=RequestContext(request,context)
	)


def getCode():

	filepath = './lighthouse/database/slepc_eprob/work_dir/eigenvalue_parallel.c'
	
	if os.path.isfile(filepath):
	   	with open(filepath, 'r') as f:
			code = f.read()
	
	return code

###---------------- Ajax post to clear request.session['selectedRoutines']------------------###
@csrf_exempt
def clear_session(request):
	if request.is_ajax():
		request.session['selectedRoutines'] = []
		return HttpResponse('All cleared!')		
	else:
		return HttpResponse('only AJAX requests are allowed!')
	


###---------------- Ajax post to remove ONE routine from request.session['selectedRoutines']------------------###
@csrf_exempt
def remove_session(request):
	if request.is_ajax():
		mode = [{'routine': request.POST.get('routine'),}]
		routineName = mode[0]['routine'][0:]
		for i, item in enumerate(request.session['selectedRoutines']):
			if item.get('routineName') == routineName and item.get('thePrecision') == rouitnePrecision:
				del request.session['selectedRoutines'][i]
				
		### important: mark the session as modified for it to save		
		request.session.modified = True

		return HttpResponse('Removed '+routineName)		
	else:
		return HttpResponse('only AJAX requests are allowed!')