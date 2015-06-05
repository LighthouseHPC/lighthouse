# Create your views here.
#!#
#!# Future considerations: What if the user skips a step in the sequence?
#!#
import os
from django.http import HttpResponse
from django.shortcuts import render_to_response, redirect, render
from django.template import RequestContext
from django.views.decorators.csrf import csrf_exempt
from lighthouse.forms.slepc_eprob import *
from lighthouse.models.slepc_eprob import *

def slepc_eprob(request):

	context = {
		'form' : problemClassForm(),
	}

	return render_to_response(
		'/lighthouse/slepc_eprob/index.html',
		context_instance=RequestContext(request,context)
	)


def guidedSearch_class(request):
	form_pClass = problemClassForm(request.POST or None)

	if form_pClass.is_valid():
		selected = form_pClass.cleaned_data['problemClass']

		if selected == u'eps':
			form = SlepcGuidedForm()
			request.session['Question_Class'] = u'Linear Eigenvalue Problem (Ax=\u03bbx, Ax=\u03bbBx)'
			action = '/slepc_eprob/guided/eps'

		elif selected == u'pep':
			form = problemTypeFormPEP()
			request.session['Question_Class'] = u'Polynomial Eigenvalue Problem [(A\u2080+\u03bbA\u2081+\u03bb\u207fA\u2099)x=0]'
			action = '/slepc_eprob/guided/pep'

		elif selected == u'nep':
			form = definitionFormNEP()
			request.session['Question_Class'] = u'Nonlinear Eigenvalue Problem (T(\u03bb)x=0)'
			action = '/slepc_eprob/guided/nep'

		context = {
			'query_class': request.session['Question_Class'],
			'form' : form,
			'Action' : action,
		}

		return render_to_response(
		'lighthouse/slepc_eprob/problem.html',
		context_instance=RequestContext(request,context)
		)

def slepc_eprob_eps(request):

    if request.method == 'POST':
        form_eps = SlepcGuidedForm(request.POST)
        data = []
        message = form_eps.errors#request.POST#
        #type = request.POST.get('type')
        if form_eps.is_valid():
            data = form_eps.cleaned_data
            message = form_eps.cleaned_data
        request.session['form'] = form_eps
        request.session['results'] = getSelectedRoutines(data)

    
    else:
        message = 'Checking'
        request.session['form'] = SlepcGuidedForm()
        request.session['results'] = []

    request.session['selectedRoutines'] = []
    #remove later
    context = {
    	'query_class': request.session['Question_Class'],
        'form'     : request.session['form'],
        'results'  : request.session['results'],
        #'message'  : request.session['message'],
        'selectedRoutines':request.session['selectedRoutines'] 
    }

    return render_to_response(
            'lighthouse/slepc_eprob/problem.html', 
        context_instance=RequestContext(request,context)
    )
def slepc_eprob_pep(request):
    if request.method == 'POST':
        form_pep = problemTypeFormPEP(request.POST)
        data = []
        message = form_pep.errors#request.POST#
        #type = request.POST.get('type')
        if form_pep.is_valid():
            data = form_pep.cleaned_data
            message = form_pep.cleaned_data
            data = data['problemTypePEP']

            if data == u'general':
            	request.session['Question_Type'] = u'General Problem'

            elif data == u'hermitian':
            	request.session['Question_Type'] = u'Hermitian (Symmetric) Problem'

            elif data == u'gyroscopic':
            	request.session['Question_Type'] = u'Gyroscopic Problem'

            elif data == u'hyperbolic':
            	request.session['Question_Type'] = u'Hyperbolic Problem'

            elif data == u'overdamped':
            	request.session['Question_Type'] = u'Overdamped Problem'

        	
        request.session['form'] = polynomialDegreeFormPEP()
        request.session['results'] = []
        action = '/slepc_eprob/guided/pep/degree'

    
    else:
        message = 'Checking'
        request.session['form'] = problemTypeFormPEP()
        request.session['results'] = []

    request.session['selectedRoutines'] = []
    #remove later
    context = {
    	'query_class': request.session['Question_Class'],
    	'query_type' : request.session['Question_Type'],
        'form'     : request.session['form'],
        'results'  : request.session['results'],
        'Action' : action,
        #'message'  : request.session['message'],
        'selectedRoutines':request.session['selectedRoutines'] 
    }

    return render_to_response(
            'lighthouse/slepc_eprob/type.html', 
        	context_instance=RequestContext(request,context)
    	)

def slepc_eprob_nep(request):
	if request.method == 'POST':
		form_nep = definitionFormNEP(request.POST)
		data = []
		message = form_nep.errors#request.POST#
        #type = request.POST.get('type')
		if form_nep.is_valid():
			data = form_nep.cleaned_data
			message = form_nep.cleaned_data
			data = data['definitionNEP']
			if data == u'callbacks':
				request.session['Question_Type'] = u'With callbacks functions for T(\u03bb) and T\'(\u03bb)'

			elif data == u'split':
				request.session['Question_Type'] = u'In split form (T(\u03bb) = \u03a3A\u1d62f\u1d62(\u03a3) and T\'(\u03bb) = \u03a3A\u1d62f\'\u1d62(\u03a3))'

        	
		request.session['form'] = numEPFormNEP()
		request.session['results'] = []
		action = '/slepc_eprob/guided/nep/numep'

    
	else:
		message = 'Checking'
		request.session['form'] = definitionFormNEP()
		request.session['results'] = []

	request.session['selectedRoutines'] = []
    #remove later
	context = {
		'query_class': request.session['Question_Class'],
		'query_type' : request.session['Question_Type'],
		'form'     : request.session['form'],
		'results'  : request.session['results'],
		'Action' : action,
		#'message'  : request.session['message'],
		'selectedRoutines':request.session['selectedRoutines'] 
	}

	return render_to_response(
		'lighthouse/slepc_eprob/type.html', 
		context_instance=RequestContext(request,context)
	)

		
def slepc_eprob_degree_pep(request):

    if request.method == 'POST':
        form_eps_degree = polynomialDegreeFormPEP(request.POST)
        data = []
        message = form_eps_degree.errors#request.POST#
        #type = request.POST.get('type')
        if form_eps_degree.is_valid():
            data = form_eps_degree.cleaned_data
            message = form_eps_degree.cleaned_data
            data = data['polynomialDegreePEP']
            if data == u'a':
                request.session['Question_Degree'] = u'Non-Quadratic'

            elif data == u'q':
                request.session['Question_Degree'] = u'Quadratic'

        request.session['form'] = miscFormPEP()
        request.session['results'] = []
        action = '/slepc_eprob/guided/pep/misc'

    
    else:
        message = 'Checking'
        request.session['form'] = polynomialDegreeFormPEP()
        request.session['results'] = []

    request.session['selectedRoutines'] = []
    #remove later
    context = {
    	'query_class': request.session['Question_Class'],
    	'query_type' : request.session['Question_Type'],
    	'query_degree' : request.session['Question_Degree'],
        'form'     : request.session['form'],
        'results'  : request.session['results'],
        'Action' : action,
        #'message'  : request.session['message'],
        'selectedRoutines':request.session['selectedRoutines'] 
    }

    return render_to_response(
            'lighthouse/slepc_eprob/degree.html', 
        context_instance=RequestContext(request,context)
    )

def slepc_eprob_misc_pep(request):

    if request.method == 'POST':
        form_eps_misc = miscFormPEP(request.POST)
        data = []
        message = form_eps_misc.errors#request.POST#
        #type = request.POST.get('type')
        if form_eps_misc.is_valid():
            data = form_eps_misc.cleaned_data
            message = form_eps_misc.cleaned_data

        request.session['form'] = form_eps_misc
        request.session['results'] = []

    
    else:
        message = 'Checking'
        request.session['form'] = miscFormPEP()
        request.session['results'] = []

    request.session['selectedRoutines'] = []
    #remove later
    context = {
    	'query_class': request.session['Question_Class'],
    	'query_type' : request.session['Question_Type'],
    	'query_degree' : request.session['Question_Degree'],
        'form'     : request.session['form'],
        'results'  : request.session['results'],
        #'message'  : request.session['message'],
        'selectedRoutines':request.session['selectedRoutines'] 
    }

    return render_to_response(
            'lighthouse/slepc_eprob/degree.html', 
        context_instance=RequestContext(request,context)
    )

def slepc_eprob_numep_nep(request):

    if request.method == 'POST':
        form_nep_numep = numEPFormNEP(request.POST)
        data = []
        message = form_nep_numep.errors#request.POST#
        #type = request.POST.get('type')
        if form_nep_numep.is_valid():
            data = form_nep_numep.cleaned_data
            message = form_nep_numep.cleaned_data
            data = data['numEPNEP']
            if data == u'y':
            	request.session['form'] = miscForm2NEP
            	request.session['Question_Degree'] = u'yes'
            elif data == u'n':
        		request.session['form'] = miscForm1NEP
        		request.session['Question_Degree'] = u'no'

        request.session['results'] = []
        action = '/slepc_eprob_misc_nep'

    
    else:
        message = 'Checking'
        request.session['form'] = miscFormPEP()
        request.session['results'] = []

    request.session['selectedRoutines'] = []
    #remove later
    context = {
    	'query_class': request.session['Question_Class'],
    	'query_type' : request.session['Question_Type'],
    	'query_degree' : request.session['Question_Degree'],
        'form'     : request.session['form'],
        'results'  : request.session['results'],
        #'message'  : request.session['message'],
        'selectedRoutines':request.session['selectedRoutines'] 
    }

    return render_to_response(
            'lighthouse/slepc_eprob/degree.html', 
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

        request.session['method'] = request.POST.get('routineName')
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
    		'query_class': request.session['Question_Class'],
            'form': request.session['form'],
            'results'  : request.session['results'],
            'selectedRoutines':request.session['selectedRoutines'],
            #'message': request.session['message'], # debug purpose use
            'code': code,
            'script':script, # Have not added makefile check if needed
          }
    return render_to_response(
            'lighthouse/slepc_eprob/problem.html', 
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
            if item.get('routineName') == routineName:
                del request.session['selectedRoutines'][i]
                
        ### important: mark the session as modified for it to save      
        request.session.modified = True

        return HttpResponse('Removed '+routineName)     
    else:
        return HttpResponse('only AJAX requests are allowed!')
