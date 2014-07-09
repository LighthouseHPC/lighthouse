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

    request.session['routineSelected'] = []
    #remove later
    context = {
                'form'     : request.session['form'],
                'results'  : request.session['results'],
                #'message'  : request.session['message'],
		'routineSelected':request.session['routineSelected'] 
    }

    return render_to_response(
            'lighthouse/slepc_eprob/index.html', 
	    context_instance=RequestContext(request,context)
    )

@csrf_exempt
def update_slepc_session(request):
	if request.is_ajax():
		#alert('hi');
		selectedRoutineNames = []
		selectedRoutineList = [{
			"routine": request.POST.get('routineName'),
		}]
		request.session['method'] = request.POST.get('routineName');
		if selectedRoutineList[0] not in request.session['routineSelected']:
			request.session['routineSelected'] = request.session['routineSelected'] + selectedRoutineList

		return HttpResponse('Dropped ')
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
			'routineSelected':request.session['routineSelected'],
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
