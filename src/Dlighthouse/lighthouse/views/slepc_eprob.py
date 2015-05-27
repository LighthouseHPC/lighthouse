# Create your views here.
import os
from django.http import HttpResponse
from django.shortcuts import render_to_response, redirect, render
from django.template import RequestContext
from django.views.decorators.csrf import csrf_exempt
from lighthouse.forms.slepc_eprob import *
from lighthouse.models.slepc_eprob import *

form_order = ('problemClassForm','SlepcGuidedForm')
'''
### help functions
def find_nextForm(currentForm_name, request):
    print request.session['form_order']
    current_index = request.session['form_order'].index(currentForm_name)
    nextForm_name = ""        
    nextForm = ""
    
    try: 
        ## search for 'none' and return the first column that has zero to be the next question/form
        next_index = next(i for i in range(current_index+1, len(request.session['form_order'])))
        nextForm_name = request.session['form_order'][next_index]
        print nextForm_name
        nextForm = getattr(sys.modules[__name__], nextForm_name)()
    ## the end of the guided search or other errors
    except Exception as e:          
        print type(e)
        print "e.message: ", e.message
        print "e.args: ", e.args
    
    return {'nextForm_name': nextForm_name, 'nextForm': nextForm}

    ### set up initial sessions
def sessionSetup(request):
    for item in ['problemClassForm','SlepcGuidedForm']:
        key = 'sylvester_'+item[:-1]
        request.session[key] = ''
    request.session['currentForm_name'] = 'problemClassForm'
    request.session['Results'] = slepc_eprob.objects.all()
    request.session['slepc_eprob_guided_answered'] = OrderedDict()
    request.session['form_order'] = []

'''
def slepc_eprob(request):

    if request.method == 'POST':
        form = problemClassForm(request.POST)
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
        request.session['form'] = problemClassForm()
        request.session['results'] = []

    request.session['selectedRoutines'] = []
    #remove later
    context = {
        'form'     : request.session['form'],
        'results'  : request.session['results'],
        #'message'  : request.session['message'],
        #'notSelectedRoutines': request.session['notSelectedRoutines'],
		'selectedRoutines':request.session['selectedRoutines'] 
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