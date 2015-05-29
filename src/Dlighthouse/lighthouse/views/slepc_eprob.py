# Create your views here.
import os
from django.http import HttpResponse
from django.shortcuts import render_to_response, redirect, render
from django.template import RequestContext
from django.views.decorators.csrf import csrf_exempt
from lighthouse.forms.slepc_eprob import *
from lighthouse.models.slepc_eprob import *

form_order_eps = ('problemClassForm','SlepcGuidedForm')
form_order_pep = ('problemClassForm','polynomialDegreeFormPEP','problemTypeFormPEP','miscPEP')
form_order_nep = ('problemClassForm')
form_order_svd = ('problemClassForm')
form_order_mfn = ('problemClassForm')
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

    ### start guided search views
def index(request):
    # set up session keys and values
    sessionSetup(request)
    
    ## get ready for the template
    context = {
                'formHTML': "problemClassForm",
                'form': "invalid",
                'slepc_eprob_guided_answered' : '',
                'results' : 'start',
    }
    return render_to_response('lighthouse/slepc_eprob/index.html', context_instance=RequestContext(request, context))

def guidedSearch(request):
    form = getattr(sys.modules[__name__], request.session['currentForm_name'])(request.GET or None)
        
    if form.is_valid():
        ## get current question and user's answer
        current_question = request.session['currentForm_name'][:-4]
        formField_name = 'sylvester_'+current_question
        value = form.cleaned_data[formField_name]
        choices = form.fields[formField_name].choices        
        request.session['sylvester_guided_answered'].update(question_and_answer(form, value, choices))

        ## generate a session for current question/answer -->request.session[sylvester_currentQuestion] = answer
        request.session[formField_name] = value
        
        
        ## decide which form order to use
        if request.session['currentForm_name'] == 'standardGeneralizedForm' and request.session['sylvester_standardGeneralized'] == 'standard':
            request.session['form_order'] = form_order_standard
        elif request.session['currentForm_name'] == 'standardGeneralizedForm' and request.session['sylvester_standardGeneralized'] == 'generalized':
            request.session['form_order'] = form_order_generalized

        
        if request.session['sylvester_standardCondition'] == 'no' or request.session['sylvester_generalizedCondition'] == 'no':         ## stop search
            return index(request)
        else:
            ## do search based on user's response (no search needed for 'standardConditionForm', 'generalizedConditionForm')
            if request.session['currentForm_name'] not in ['standardConditionForm', 'generalizedConditionForm']:
                lookup = "%s__contains" % current_question
                query = {lookup : value}
                request.session['Results'] = request.session['Results'].filter(**query)
            
            ## call function find_nextForm to set up next form for next question
            dict_nextQuestion = find_nextForm(request.session['currentForm_name'], request)           
            nextForm_name = dict_nextQuestion['nextForm_name']
            nextForm = dict_nextQuestion['nextForm']
            
            ## make next form current for request.session['currentForm_name']
            request.session['currentForm_name'] = nextForm_name
            
            ## decide whether or not to use form HTML files (if help buttons are needed, use HTML file instead of form)
            if nextForm_name in form_HTML:
                formHTML = nextForm_name
            else:
                formHTML = "invalid"
            
            ## get ready for the template       
            context = {
                        'formHTML': formHTML,
                        'form': nextForm,
                        'sylvester_guided_answered' : request.session['sylvester_guided_answered'],
                        'results' : request.session['Results']
                        }
            return render_to_response('lighthouse/lapack_sylvester/index.html', context_instance=RequestContext(request, context))
    else:       
        return index(request)
    

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