import string, types, sys, os
from django.http import HttpResponse
from django.template import RequestContext
from django.shortcuts import render_to_response, redirect, render
from django.template.loader import render_to_string
#---- for method = 'post' ---#
from django.views.decorators.csrf import csrf_exempt


from Driver.models import RoutineInfo, LinearEquation_simple, LinearEquation_expert 
from Computational.models import LinearEquation_computational
from Combine.models import LinearEquation_only
from Driver.forms import ProblemForm, EquationForm, FactorForm, PrecisionForm, ComplexForm, MatrixTypeForm, StorageForm, AdvancedForm, LinearEquation_computationalForm, LinearEquation_simpleForm, LinearEquation_expertForm, scriptForm

from django.contrib.contenttypes.models import ContentType
from django.db.models import get_model, Q
from itertools import chain
from haystack.query import SearchQuerySet

from django.core.urlresolvers import reverse
from Codegen.templates import BTOGenerator

from django.contrib.auth.decorators import login_required

import StringIO
from django.core.servers.basehttp import FileWrapper



###-------------------- Notes ----------------------###
'''
(1) "request.session" is a BIG dictionary that records the answers --> request.session = {Routines: [], 'Question_problem': [], 'Question_equation': [], 'Question_factor':[], ...}
(2) form.cleaned_data and request.POST.getlist do the same thing --> list the chosen options.
(3) get_model('appName', 'modelName') --> remember the ' '!
(4) MODEL.objects.filter(**condition) --> condition MUST be a dictionary.
(5) Model_List=[]  ---> when calling more than one models for SearchQuerySet().
'''



### ---------------- Define functions------------------ ###
#Combine (and) the Q's.
def combine_Q(aList):
	query = Q()
	for value in aList:
		query &= value
	return query	

###---------------- Ajax post to update request.session['selectedRoutines']------------------###
@csrf_exempt
def update_session(request):
	if request.is_ajax():
		selectedRoutineNames = []
		selectedRoutineList = [
			{"thePrecision": request.POST.get('precision'),
			 "routineName": request.POST.get('routineName'),
			 "matrixType": request.POST.get('matrixType'),
			 "storageType": request.POST.get('storageType'),
			 "id": request.POST.get('idn'),
			 "url": request.POST.get('url'),
			 "serialNo": 0,
			 "checkState": request.POST.get('checkState')}
			]
		
		counter = 0
		match = -1
		for item in request.session['selectedRoutines']:
			if item['thePrecision'] == selectedRoutineList[0]['thePrecision'] and item['routineName'] == selectedRoutineList[0]['routineName']:
				match = counter
				if selectedRoutineList[0]['checkState'] == 'checked':
					request.session['selectedRoutines'][counter]['checkState'] = 'checked'
				if selectedRoutineList[0]['checkState'] == 'unchecked':
					request.session['selectedRoutines'][counter]['checkState'] = 'unchecked'							
			counter += 1

		if match == -1:
			selectedRoutineList[0]['serialNo'] = getSerialNo(request, selectedRoutineList[0]['thePrecision'], selectedRoutineList[0]['routineName'])
			request.session['selectedRoutines'] = request.session['selectedRoutines'] + selectedRoutineList

		request.session.modified = True
			
		for item in request.session['selectedRoutines']:
			if item['checkState'] == 'checked':
				selectedRoutineNames.append(item['thePrecision']+item['routineName']+',')

		return HttpResponse(selectedRoutineNames)
	else:
		return HttpResponse('only AJAX requests are allowed!')
	

def getSerialNo(request, precision, routineName):

	i = 0
	for item in request.session['selectedRoutines']:	
		i += 1
		if item['thePrecision'] == precision and item['routineName'] == routineName:
			return request.session['selectedRoutines'][i]['serialNo']

	serialNo = len(request.session['selectedRoutines']) + 1		
	return serialNo

def filterSelectedRoutines(request):

	request.session['notSelectedRoutines'] = request.session['Routines']

	for item in request.session['selectedRoutines']:
		request.session['notSelectedRoutines'] = request.session['notSelectedRoutines'].exclude(Q(thePrecision=item['thePrecision']), Q(routineName=item['routineName']))	
	
	request.session.modified = True

def filterSelectedRoutines2(request, routines):

	indices = []
	i = 0
	for item1 in routines:		
		for item2 in request.session['selectedRoutines']:
			if item2['thePrecision'] == item1.thePrecision and item2['routineName'] == item1.routineName:
				indices.append(i)
		i += 1

	indices.reverse()
	for item in indices:
		routines.pop(item)
	
	return routines

###---------------- Ajax post to clear request.session['selectedRoutines']------------------###
@csrf_exempt
def clear_session(request):
	if request.is_ajax():
		mode = [{"clear": request.POST.get('clear')}]
		if mode[0]['clear'] == 'all':
			request.session['selectedRoutines'] = []
			return HttpResponse('cleared')
		elif mode[0]['clear'] == 'unchecked':
			test = request.session['selectedRoutines']
			request.session['selectedRoutines'] = []
			for item in test:
				if item['checkState'] == 'checked':					
					request.session['selectedRoutines'].append(item)
		elif mode[0]['clear'] == 'checked':
			test = request.session['selectedRoutines']
			request.session['selectedRoutines'] = []
			for item in test:
				if item['checkState'] == 'unchecked':					
					request.session['selectedRoutines'].append(item)
			return HttpResponse('cleared')				
	else:
		return HttpResponse('only AJAX requests are allowed!')		




###---------------- Guided Search ------------------###
#Question_problem: Which of the following functions do you wish to execute?
#@login_required
def search_forms(request):    
	try:
		request.session['selectedRoutines']
	except KeyError:
	    request.session['selectedRoutines'] = []	
	except NameError:
		request.session['selectedRoutines'] = []

  	context = {'form': ProblemForm(), 'formAdvanced': AdvancedForm(), 'scriptForm': scriptForm(), 
  	'selectedRoutines': request.session['selectedRoutines'], 'codeTemplate': getCodeTempate(request.session.session_key)}
	return render_to_response('search/index.html', context_instance=RequestContext(request, context))

#Question_problem answered!
#Question_equation: What form of the linear system do you want to solve? 
#or 
#Question_complex: Are there complex numbers in your matrix?
@csrf_exempt
def guidedSearch_problem(request):
        form_Prob = ProblemForm(request.POST or None)
        request.session['Question_problem'] = []
        request.session['queries'] = []
	
	try:
	  request.session['selectedRoutines']
	except KeyError:
	  request.session['selectedRoutines'] = []

        if form_Prob.is_valid():
                selected = form_Prob.cleaned_data['question_prob']
                for answer in selected:
                        request.session['Question_problem'].append((answer, ProblemForm().find(answer))) 

                appName = selected[0].split()[0]
                modelName = selected[0].split()[1]
                for item in selected:
                        request.session['queries'].append(Q(notes__icontains=item.split()[2]))

                request.session['Routines'] = get_model(appName,modelName).objects.filter(combine_Q(request.session['queries']))
                filterSelectedRoutines(request)

                if appName == 'Driver' or appName == 'Combine':
                        form = EquationForm()
                        action = '/search/guided/problem_equation/'
        
                else:
                        form = ComplexForm()
                        action = '/search/guided/problem_complex/'
                        request.session['Question_equation']=[0, 0]
                        request.session['Question_factor']=[0, 0]       

                context = {'query_prob': request.session['Question_problem'], 'form': form, 'Action': action, 'results': request.session['Routines'], 'notSelectedRoutines': request.session['notSelectedRoutines'], 'selectedRoutines': request.session['selectedRoutines'], 'codeTemplate': getCodeTempate(request.session.session_key)}
                return render_to_response('search/problem.html', context_instance=RequestContext(request, context))        

        else:
                context = {'form': ProblemForm(), 'selectedRoutines': request.session['selectedRoutines'], 'codeTemplate': getCodeTempate(request.session.session_key)}
                return render_to_response('search/index.html', context_instance=RequestContext(request, context))




#Question_equation answered.
#Question_factor: Is your matrix factored?
def guidedSearch_equation(request):
	form_Equa = EquationForm(request.POST or None)
	if form_Equa.is_valid():
		if form_Equa.cleaned_data['question_equa'] == unicode('transpose'):
			val_0 = 'transpose'
			val_1 = 'A<sup>T</sup>X = B'
			complex_initial_value = 'n'
			request.session['Routines'] = request.session['Routines'].filter(notes__icontains='trans').filter(Q(thePrecision='s')|Q(thePrecision='d'))

		elif form_Equa.cleaned_data['question_equa'] == unicode('Hermitian_trans'):
			val_0 = 'Hermitian_trans'
			val_1 = 'A<sup>H</sup>X = B'
			complex_initial_value = 'y'	
			request.session['Routines'] = request.session['Routines'].filter(notes__icontains='trans').filter(Q(thePrecision='c')|Q(thePrecision='z'))

		else:
			val_0 = 'original'
			val_1 = 'AX = B'
			complex_initial_value = 'None'
			if 'Solve a linear equation only' in request.session['Question_problem'][0]:
				request.session['Routines'] = get_model('Combine', 'LinearEquation_only').objects.filter(Q(notes__icontains='simple')|Q(notes__icontains='computational'))
				

		request.session['Question_equation'] = [val_0, val_1] 
		request.session['Complex_initial'] = complex_initial_value
		form = FactorForm()
		filterSelectedRoutines(request)

		context = {'query_prob': request.session['Question_problem'], 'query_equa': val_1, 'form': form,
			   'results': request.session['Routines'], 'notSelectedRoutines': request.session['notSelectedRoutines'], 'selectedRoutines': request.session['selectedRoutines'], 'codeTemplate': getCodeTempate(request.session.session_key)}					
		return render_to_response('search/equation.html', context_instance=RequestContext(request, context))

 			
	else:
        	form = EquationForm()
		action = '/search/problem/equation/'
		context = {'query_prob': request.session['Question_problem'], 'form': form, 'Action': action,
			   'results': request.session['Routines'], 'selectedRoutines': request.session['selectedRoutines'], 'codeTemplate': getCodeTempate(request.session.session_key)}
        	return render_to_response('search/problem.html', context_instance=RequestContext(request, context))





#Question_factor answered.
#Question_complex: Are there complex numbers in your matrix?
def guidedSearch_factor(request):
	form_Fact = FactorForm(request.POST or None)
	if form_Fact.is_valid():
		for val in form_Fact.fields['question_fact'].choices:
			if val[0] == form_Fact.cleaned_data['question_fact']:
				request.session['Question_factor'] = [val[0], val[1]]
				
		if form_Fact.cleaned_data['question_fact'] == unicode('y'):
			request.session['FACT'] = 'Y'
			if 'Solve a linear equation only' in request.session['Question_problem'][0]:
				request.session['Routines'] = request.session['Routines'].filter(notes__icontains='computational')
			else:
				request.session['Routines'] = request.session['Routines'].filter(notes__icontains='expert')

		else:
			request.session['FACT'] = 'N'
			if 'Solve a linear equation only' in request.session['Question_problem'][0] and request.session['Question_equation'][0] == 'original':
				request.session['Routines'] = request.session['Routines'].filter(notes__icontains='simple') 
			else:
				request.session['Routines'] = request.session['Routines'].filter(notes__icontains='expert')

		form = ComplexForm(initial=dict(question_comp=request.session['Complex_initial']))
		filterSelectedRoutines(request)

		context = {'query_prob': request.session['Question_problem'], 'query_equa': request.session['Question_equation'][1],
			   'query_fact': request.session['Question_factor'][1], 'form': form, 'results': request.session['Routines'], 'notSelectedRoutines': request.session['notSelectedRoutines'],
			   'selectedRoutines': request.session['selectedRoutines'], 'codeTemplate': getCodeTempate(request.session.session_key)}					
		return render_to_response('search/factor.html', context_instance=RequestContext(request, context))

 			
	else:
        	form = FactorForm()
		context = {'query_prob': request.session['Question_problem'], 'query_equa': request.session['Question_equation'][1],
			   'form': form, 'results': request.session['Routines'], 'selectedRoutines': request.session['selectedRoutines'], 'codeTemplate': getCodeTempate(request.session.session_key)}
        	return render_to_response('search/equation.html', context_instance=RequestContext(request, context))






#Question_complex answered.
#Question _matrixType: What is the type of your matrix? 
def guidedSearch_complex(request):
	form_Comp = ComplexForm(request.POST or None)
	if form_Comp.is_valid():
		if form_Comp.cleaned_data['question_comp'] == unicode('y'):
			val_0 = 'y'
			val_1 = 'yes'
			request.session['Routines'] = request.session['Routines'].filter(**{'thePrecision__in': ['c', 'z']})
		else:
			val_0 = 'n'
			val_1 = 'no'
			request.session['Routines'] = request.session['Routines'].filter(**{'thePrecision__in': ['s', 'd']})
			
		request.session['Question_complex'] = [val_0, val_1] 
		form = MatrixTypeForm(request)
		filterSelectedRoutines(request)	
		context = {'query_prob': request.session['Question_problem'], 'query_equa': request.session['Question_equation'][1],
			   'query_fact': request.session['Question_factor'][1], 'query_comp': val_1, 'form': form,
			   'results': request.session['Routines'], 'notSelectedRoutines': request.session['notSelectedRoutines'], 'selectedRoutines': request.session['selectedRoutines'],'codeTemplate': getCodeTempate(request.session.session_key) }					
		
		return render_to_response('search/complex.html', context_instance=RequestContext(request, context))

 			
	else:
        	form = ComplexForm()
		if request.session['Question_equation']==[0, 0]:
			action = '/search/problem/complex/'
			context = {'query_prob': request.session['Question_problem'], 'form': form, 'Action': action, 'results': request.session['Routines']}		
			return render_to_response('search/problem.html', context_instance=RequestContext(request, context))
		else:
			context = {'query_prob': request.session['Question_problem'], 'query_equa': request.session['Question_equation'][1],
				   'query_fact': request.session['Question_factor'][1],'form': form, 'results': request.session['Routines'],
				   'selectedRoutines': request.session['selectedRoutines'], 'codeTemplate': getCodeTempate(request.session.session_key)}
        		return render_to_response('search/factor.html', context_instance=RequestContext(request, context))





#Question_matrixtype answered. 
#Question_storageType: How is your matrix stored?

def guidedSearch_matrixtype(request):
        form_Type = MatrixTypeForm(request, request.POST or None)
	if form_Type.is_valid():
 		for val in form_Type.fields['question_type'].choices:
			if val[0] == form_Type.cleaned_data['question_type']:
				request.session['Routines'] = request.session['Routines'].filter(matrixType = val[0])
				request.session['Question_matrixtype'] = [val[0], val[1]]
				form = StorageForm(request)
				filterSelectedRoutines(request)
				context = {'query_prob': request.session['Question_problem'],  'query_equa': request.session['Question_equation'][1],
					   'query_fact': request.session['Question_factor'][1], 'query_comp': request.session['Question_complex'][1],
					   'query_type': val[1], 'form': form, 'results': request.session['Routines'], 'notSelectedRoutines': request.session['notSelectedRoutines'],
					   'selectedRoutines': request.session['selectedRoutines'],'codeTemplate': getCodeTempate(request.session.session_key) }
				return render_to_response('search/matrixtype.html', context_instance=RequestContext(request, context)) 

	
	else:
		form = MatrixTypeForm(request)
		context = {'query_prob': request.session['Question_problem'], 'query_equa': request.session['Question_equation'][1],
			   'query_fact': request.session['Question_factor'][1], 'query_comp': request.session['Question_complex'][1],
			   'form': form, 'results': request.session['Routines'], 'selectedRoutines': request.session['selectedRoutines'],}
		return render_to_response('search/complex.html', context_instance=RequestContext(request, context))







#Question_storageType answered.
#Question_thePrecision: Would you like to use single or double precision?
def guidedSearch_storage(request):
        form_Stor = StorageForm(request, request.POST or None)
	if form_Stor.is_valid():
 		for val in form_Stor.fields['question_stor'].choices:
			if val[0] == form_Stor.cleaned_data['question_stor']:
				request.session['Routines'] = request.session['Routines'].filter(storageType = val[0])
				request.session['Question_storagetype'] = [val[0], val[1]]
				form = PrecisionForm()
				filterSelectedRoutines(request)
				context = {'query_prob': request.session['Question_problem'], 'query_equa': request.session['Question_equation'][1],
					   'query_fact': request.session['Question_factor'][1], 'query_comp': request.session['Question_complex'][1],
					   'query_type': request.session['Question_matrixtype'][1], 'query_stor': val[1], 'form': form,
					   'results': request.session['Routines'], 'notSelectedRoutines': request.session['notSelectedRoutines'], 'selectedRoutines': request.session['selectedRoutines'], 'codeTemplate': getCodeTempate(request.session.session_key)}
				return render_to_response('search/storagetype.html', context_instance=RequestContext(request, context)) 

	
	else:
		form = StorageForm(request)
		context = {'query_prob': request.session['Question_problem'], 'query_equa': request.session['Question_equation'][1],
			   'query_fact': request.session['Question_factor'][1], 'query_comp': request.session['Question_complex'][1],
			   'query_type': request.session['Question_matrixtype'][1], 'form': form,
			   'results': request.session['Routines'], 'selectedRoutines': request.session['selectedRoutines'],}
		return render_to_response('search/matrixtype.html', context_instance=RequestContext(request, context))   






#Question_thePrecisio answered. ---> Final result.
def guidedSearch_precision(request):
	form_Prec = PrecisionForm(request.POST or None)
	if form_Prec.is_valid():
		if form_Prec.cleaned_data['question_prec'] == unicode('d'):
			val_0 = 'd'	
			val_1 = 'double'
			if request.session.get('Question_complex')[0] == 'y':
				request.session['Routines'] = request.session['Routines'].filter(thePrecision = 'z')
			
			if request.session.get('Question_complex')[0] == 'n':
				request.session['Routines'] = request.session['Routines'].filter(thePrecision = 'd')

		else:
			val_0 = 's'	
			val_1 = 'single'
			if request.session.get('Question_complex')[0] == 'y':
				request.session['Routines'] = request.session['Routines'].filter(thePrecision = 'c')
			
			if request.session.get('Question_complex')[0] == 'n':
				request.session['Routines'] = request.session['Routines'].filter(thePrecision = 's')	

		filterSelectedRoutines(request)
		context = {'query_prob': request.session['Question_problem'], 'query_equa': request.session['Question_equation'][1],
			   'query_fact': request.session['Question_factor'][1], 'query_comp': request.session['Question_complex'][1],
			   'query_type': request.session['Question_matrixtype'][1], 'query_stor': request.session['Question_storagetype'][1],
			   'query_prec': val_1, 'results': request.session['Routines'], 'notSelectedRoutines': request.session['notSelectedRoutines'], 'selectedRoutines': request.session['selectedRoutines'], 'codeTemplate': getCodeTempate(request.session.session_key)}
		return render_to_response('search/precision.html', context_instance=RequestContext(request, context)) 		


	else:
		form = PrecisionForm()
		context = {'query_prob': request.session['Question_problem'], 'query_equa': request.session['Question_equation'][1],
			   'query_fact': request.session['Question_factor'][1], 'query_comp': request.session['Question_complex'][1],
			   'query_type': request.session['Question_matrixtype'][1], 'query_stor': request.session['Question_storagetype'][1],
			   'form': form, 'results': request.session['Routines'], 'selectedRoutines': request.session['selectedRoutines'], 'codeTemplate': getCodeTempate(request.session.session_key)}
		return render_to_response('search/storagetype.html', context_instance=RequestContext(request, context))











###---------------- Advanced Search ------------------###
#def advancedSearch(request):
#    request.session.clear()
#
#    form = AdvancedForm()	
#    context = {'form': form}
#
#    return render_to_response('search/advancedSearch.html', context_instance=RequestContext(request, context))



#Turen a string into a class
def str_to_class(str):
    return getattr(sys.modules[__name__], str)


#determine 's', 'd', 'c', 'z'
def whatPrecision(comp, prec):
	if comp == 'yes' and prec == 'single':
		thePrecision = 'c'
	elif comp == 'yes' and prec == 'double':
		thePrecision = 'z'
	elif comp == 'no' and prec == 'single':
		thePrecision = 's'
	else:
		thePrecision = 'd'
	return thePrecision





###---Notes---###
'''
(1) form = LinearEquation_simpleForm() --> form.fields --> {'MatrixType': <django.forms.fields.MultipleChoiceField object at 0x2028590>, 'StorageType': <django.forms.fields.MultipleChoiceField object at 0x2028610>,...}
(2) form.fields.items() --> [('MatrixType', <django.forms.fields.MultipleChoiceField object at 0x2762690>),('StorageType', <django.forms.fields.MultipleChoiceField object at 0x2762710>),('Precision', <django.forms.fields.MultipleChoiceField object at 0x2762790>), ...]
(3) to pass a certain field to the template --> form = xxxForm() --> form['fieldName'].
(4) can't pass arguments in the django template --> variables (field names) need to be handled in views.

'''


def advancedForm(request):
	form_advanced = AdvancedForm(request.POST or None)
	request.session['Question_advanced'] = []
	request.session['App_Model'] = []			#---request.session['App_Model'] is a list of tuples: [(app1, model1), (app2, model2), (),...]
	request.session['Forms'] = []
	request.session['Function'] = []
	request.session['Complex'] = []
	request.session['MatrixType'] = []
	request.session['StorageType'] = []
	request.session['Precision'] = []

	try:
	  request.session['selectedRoutines']
	except KeyError:
	  request.session['selectedRoutines'] = []	
	except NameError:
	  request.session['selectedRoutines'] = []	
		
	if form_advanced.is_valid():
		selected = form_advanced.cleaned_data['advanced']
		for answer in selected:
			request.session['Question_advanced'].append(AdvancedForm().find(answer)) 
			request.session['App_Model'].append((answer.split()[0], answer.split()[1][:-4])) 
			form = str_to_class(answer.split()[1])()
			request.session['Forms'].append(form)
			request.session['Function'].append(form[answer.split()[1][:-4]+"Function"])
			request.session['Complex'].append(form[answer.split()[1][:-4]+"Complex"])
			request.session['MatrixType'].append(form[answer.split()[1][:-4]+"MatrixType"])
			request.session['StorageType'].append(form[answer.split()[1][:-4]+"StorageType"])
			request.session['Precision'].append(form[answer.split()[1][:-4]+"Precision"])
				
		context = {'Question_advanced': request.session['Question_advanced'], 'Forms': request.session['Forms'], 
		'Function': request.session['Function'], 'Complex': request.session['Complex'], 'MatrixType':request.session['MatrixType'], 
		'StorageType':request.session['StorageType'], 'Precision': request.session['Precision'], 'selectedRoutines': request.session['selectedRoutines'], 'codeTemplate': getCodeTempate(request.session.session_key)}
		return render_to_response('search/advancedForm.html', {'AdvancedTab': True}, context_instance=RequestContext(request, context))

	else:
   		form = AdvancedForm()	
		context = {'form': form, 'selectedRoutines': request.session['selectedRoutines'], 'codeTemplate': getCodeTempate(request.session.session_key)}
		return render_to_response('search/advancedSearch.html', {'AdvancedTab': True}, context_instance=RequestContext(request, context))



	


def advancedResult(request):
#----- Display checked items -------#

	for item in ['GETS', 'EquationGETS', 'FunctionGETS', 'ComplexGETS', 'MatrixTypeGETS', 'StorageTypeGETS', 'PrecisionGETS']:  
		request.session[item] = []

	request.session['Results'] = {}
	routines = []

	for model in request.session['App_Model']:
		form_empty = str_to_class(model[1]+'Form')()
		form = str_to_class(model[1]+'Form')(request.POST or None)
		if model[1] == 'LinearEquation_expert':
			request.session['EquationGETS'].append(form[model[1]+"Equation"])
		request.session['GETS'].append(form)
		request.session['FunctionGETS'].append(form[model[1]+"Function"])
		request.session['ComplexGETS'].append(form[model[1]+"Complex"])
		request.session['MatrixTypeGETS'].append(form[model[1]+"MatrixType"])
		request.session['StorageTypeGETS'].append(form[model[1]+"StorageType"])
		request.session['PrecisionGETS'].append(form[model[1]+"Precision"])
		request.session['Results'][model[1]] = []


#----- Collect the checked data -----#
#Recored results in request.session['Routines']={modelName1:[<class>], modelName2:[<class>], ...}
		if form.is_valid():
			selected_Function = form.cleaned_data[model[1]+'Function']
			selected_Complex = form.cleaned_data[model[1]+'Complex']
			selected_MatrixType = form.cleaned_data[model[1]+'MatrixType']
			selected_StorageType = form.cleaned_data[model[1]+'StorageType']
			selected_Precision = form.cleaned_data[model[1]+'Precision']
			selected_Equation = 0
			if model[1] == 'LinearEquation_expert':
				selected_Equation = form.cleaned_data[model[1]+'Equation']
				for comp in selected_Complex:
					for precision in selected_Precision:
						for matrix in selected_MatrixType:
							for storage in selected_StorageType:
								for function in selected_Function:
									for equation in selected_Equation:
										if equation == 'solve':
											routine = LinearEquation_expert.objects.filter(
												thePrecision=whatPrecision(comp, precision),
												matrixType=matrix,
												storageType=storage,
												notes__icontains=function
											)
											request.session['Results'][model[1]].append({
												'Complex number': comp, 
												'Precision': precision, 
												'Matrix type': matrix,
												'Storage type': storage, 
												'Function': form_empty.find_function(function),
											    'Equation': form_empty.find_equation(equation), 
											    'Description': form.Description,
												'Routine': routine
											})
											routines.append(list(routine))

 										else: 
 											routine = LinearEquation_expert.objects.filter(
 												thePrecision=whatPrecision(comp, precision), 
 												matrixType=matrix, storageType=storage, 
 												notes__icontains='trans').filter(notes__icontains=function
 											)
											request.session['Results'][model[1]].append({
												'Complex number': comp, 
												'Precision': precision, 
												'Matrix type': matrix,
											    'Storage type': storage, 
											    'Function': form_empty.find_function(function),
												'Equation': form_empty.find_equation(equation), 
												'Description': form.Description,
										     	'Routine': routine
										    })
											routines.append(list(routine))

			else:
				for comp in selected_Complex:
					for precision in selected_Precision:
						for matrix in selected_MatrixType:
							for storage in selected_StorageType:
								for function in selected_Function:
									routine = get_model(*model).objects.filter(
										thePrecision=whatPrecision(comp, precision), 
										matrixType=matrix, 
										storageType=storage, 
										notes__icontains=function
									)

									request.session['Results'][model[1]].append({
										'Complex number': comp, 
										'Precision': precision, 
										'Matrix type': matrix,
									    'Storage type': storage, 
									    'Function': form_empty.find(function),
								     	'Equation': 0, 
								     	'Description': form.Description,
										'Routine': routine
									})
									routines.append(list(routine))
		
	alreadySelectedRoutines = filterSelectedRoutines3(request, routines)
	#L = []
	context = {'Question_advanced': request.session['Question_advanced'], 'GETS': request.session['GETS'], 'FunctionGETS': request.session['FunctionGETS'],
		   'ComplexGETS': request.session['ComplexGETS'], 'MatrixTypeGETS':request.session['MatrixTypeGETS'], 'StorageTypeGETS':request.session['StorageTypeGETS'],
		   'PrecisionGETS': request.session['PrecisionGETS'], 'EquationGETS': request.session['EquationGETS'], 'selected_Equation': selected_Equation, 'Results': request.session['Results'], 'alreadySelectedRoutines': alreadySelectedRoutines, 'selectedRoutines': request.session['selectedRoutines'], 'codeTemplate': getCodeTempate(request.session.session_key)}	

	return render_to_response('search/advancedResult.html', {'AdvancedTab': True}, context_instance=RequestContext(request, context))

#	else:
#   		form = AdvancedForm()	
#    		context = {'form': form}

#    		return render_to_response('search/advanced_search.html', context_instance=RequestContext(request, context))


def filterSelectedRoutines3(request, routines):

	alreadySelectedRoutines = []

	for item1 in request.session['selectedRoutines']:
		for lst in routines:
			for item2 in lst:
				if item2.thePrecision == item1['thePrecision'] and item2.routineName == item1['routineName']:
					alreadySelectedRoutines.append(item2)

	return alreadySelectedRoutines

###---------------- Keyword Search ------------------###

def keywordResult(request):

	keywords = ""

	try:
		request.session['selectedRoutines']
	except KeyError:
	    request.session['selectedRoutines'] = []	
	except NameError:
		request.session['selectedRoutines'] = []
	
	if request.GET.get('kwtb'):		
		keywords = request.GET['kwtb']
		keywords = keywords.lower().strip()
		routines = []

		# For keywords within double quotes
		if keywords.startswith('"') and keywords.endswith('"'):
			routines_le_simple = SearchQuerySet().models(LinearEquation_simple).filter(info__icontains=keywords)
			routines_le_expert = SearchQuerySet().models(LinearEquation_expert).filter(info__icontains=keywords)
			routines_le_computational = SearchQuerySet().models(LinearEquation_computational).filter(info__icontains=keywords)
		# For keywords without double quotes 
		else:			
			keyword_array = keywords.split(' ')
			formatted_keyword_array = []
			for word in keyword_array:
				formatted_keyword_array.append(word.strip())

			queries = [Q(info__icontains=word) for word in formatted_keyword_array]
			query = queries.pop()
			
			# The query is made by ANDing the keywords (doing OR produces way too many results)
			for item in queries:
			    query &= item
		
			routines_le_simple = SearchQuerySet().models(LinearEquation_simple).filter(query)
			routines_le_expert = SearchQuerySet().models(LinearEquation_expert).filter(query)
			routines_le_computational = SearchQuerySet().models(LinearEquation_computational).filter(query)
			
		routines = list(chain(routines_le_simple, routines_le_expert, routines_le_computational))
		notSelectedRoutines = filterSelectedRoutines2(request, list(chain(routines_le_simple, routines_le_expert, routines_le_computational)))
		context = {'form': ProblemForm(), 'formAdvanced': AdvancedForm(), 'scriptForm': scriptForm(), 'keywords': keywords, 'results': routines, 'notSelectedRoutines': notSelectedRoutines, 'selectedRoutines': request.session['selectedRoutines'], 'codeTemplate': getCodeTempate(request.session.session_key) }
		return render_to_response('search/keywordResult.html', {'KeywordTab': True}, context_instance=RequestContext(request, context))
	else:
		HttpResponse("Error!")







###---------------- Script ------------------###

def runScript(request):
  if request.method == 'POST':
    form = scriptForm(request.POST)
    if form.is_valid():    
      request.session['userScript'] = form.cleaned_data['script']
      #request.session['generatedCodeTemplate'] = "User input: " + request.session['userScript']
      bto = BTOGenerator()
      request.session['generatedCodeTemplate'] = bto.generateCode(str(request.session['userScript']))
      context = {'form': ProblemForm(), 'formAdvanced': AdvancedForm(), 'scriptcode': request.session['userScript'], 'scriptForm': scriptForm(initial={'script': request.session['userScript']}), 'generatedCodeTemplate': request.session['generatedCodeTemplate'], 'selectedRoutines': request.session['selectedRoutines'], 'codeTemplate': getCodeTempate(request.session.session_key)}        
      return render_to_response('search/index.html', {'ScriptTab': True}, context_instance=RequestContext(request, context))

    else:
      context = {'form': ProblemForm(), 'formAdvanced': AdvancedForm(), 'scriptForm': scriptForm(), 'selectedRoutines': request.session['selectedRoutines'], 'codeTemplate': getCodeTempate(request.session.session_key)}
      return render_to_response('search/index.html', {'ScriptTab': True}, context_instance=RequestContext(request, context))


###---------------- Code Template ------------------###

# return the content of the code template file, if exists
def getCodeTempate(session_key):

	fileName_c = './generatedCodeTemplate/' + session_key + '.c'
	fileName_f = './generatedCodeTemplate/' + session_key + '.f'

	template = ""
	
	if os.path.isfile(fileName_c):
	   	with open(fileName_c, 'r') as f:
			template = f.read()
	elif os.path.isfile(fileName_f):
	   	with open(fileName_f, 'r') as f:
			template = f.read()

	return template

def downloadTemplate(request):

	fileName_c = './generatedCodeTemplate/' + request.session.session_key + '.c'
	fileName_f = './generatedCodeTemplate/' + request.session.session_key + '.f'

	# assuming the user in not generating templates in both C and FORTRAN

	if os.path.isfile(fileName_c):
	   	filename = fileName_c
	if os.path.isfile(fileName_f):
	   	filename = fileName_f

	wrapper = FileWrapper(open(filename))
	response = HttpResponse(wrapper, content_type='text/plain')
	response['Content-Disposition'] = 'attachment; filename=%s' % filename
	return response

	return response