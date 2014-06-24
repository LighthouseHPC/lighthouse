import string, types, sys, os, StringIO, re, shlex
from django.contrib.contenttypes.models import ContentType
from django.contrib.auth.decorators import login_required
from django.core.paginator import Paginator
from django.core.servers.basehttp import FileWrapper
from django.core.urlresolvers import reverse
from django.db.models import get_model, Q
from django.http import HttpResponse
from django.shortcuts import render_to_response, redirect, render
from django.template import RequestContext
from django.template.loader import render_to_string
#---- for method = 'post' ---#
from django.views.decorators.csrf import csrf_exempt

from lighthouse.codeGen.templates import BTOGenerator

from haystack.forms import ModelSearchForm
from haystack.inputs import AutoQuery, Exact, Clean
from haystack.views import SearchView
from haystack.query import SearchQuerySet

from itertools import chain

from lighthouse.forms.lapack_le import *
from lighthouse.models.lapack_le import *

from spell.spell import correct





###-------------------- Notes ----------------------###
'''
(1) "request.session" is a BIG dictionary that records the answers --> request.session = {Routines: [], 'Question_problem': [], 'Question_equation': [], 'Question_factor':[], ...}
(2) form.cleaned_data and request.POST.getlist do the same thing --> list the chosen options.
(3) get_model('appName', 'modelName') --> remember the ' '!
(4) MODEL.objects.filter(**condition) --> condition MUST be a dictionary.
(5) Model_List=[]  ---> when calling more than one models for SearchQuerySet().
'''



### ---------------- Define help functions------------------ ###
#Combine (and) the Q's.
def combine_Q(aList):
	query = Q()
	for value in aList:
		query &= value
	return query	
	




###---------------- Guided Search ------------------###
#Question_problem: Which of the following functions do you wish to execute?
#@login_required(login_url='/')
def search_forms(request):    
	try:
		request.session['selectedRoutines']
		request.session['scriptOutput']
		request.session['userScript']	
	except (NameError,KeyError):
		request.session['selectedRoutines'] = []
		request.session['scriptOutput'] = ""
		request.session['userScript'] = ""

  	context = {
  		'form': ProblemForm(), 
  		'formAdvanced': AdvancedForm(), 
  		'scriptForm': scriptForm(), 
  		'selectedRoutines': request.session['selectedRoutines'], 
  		'scriptCode': request.session['userScript'], 
  		'scriptOutput': request.session['scriptOutput'],
		'results': 'start',
  	}
	return render_to_response(
		'lighthouse/lapack_le/index.html', 
		context_instance=RequestContext(request, context)
	)

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
		request.session['scriptOutput']
		request.session['userScript']	
	except (NameError,KeyError):
		request.session['selectedRoutines'] = []
		request.session['scriptOutput'] = ""
		request.session['userScript'] = ""

        if form_Prob.is_valid():
                selected = form_Prob.cleaned_data['question_prob']
		## remove Radiobutton 8: id="id_question_prob_7
		if 'Driver lapack_le_expert' in selected:
			selected.remove('Driver lapack_le_expert')
			
		## get "value"	
                for answer in selected:
			print answer
                        request.session['Question_problem'].append((answer, ProblemForm().find(answer))) 

                cataName = selected[0].split()[0]
                modelName = selected[0].split()[1]
                for item in selected:
                        request.session['queries'].append(Q(notes__icontains=item.split()[2]))

                request.session['Routines'] = get_model('lighthouse',modelName).objects.filter(combine_Q(request.session['queries']))

                filterSelectedRoutines(request)

                if cataName == 'Driver' or cataName == 'Combine':
                        form = EquationForm()
                        action = '/lapack_le/guided/problem_equation/'
        
                else:
                        form = ComplexForm()
                        action = '/lapack_le/guided/problem_complex/'
                        request.session['Question_equation']=[0, 0]
                        request.session['Question_factor']=[0, 0]       

                context = {
                	'query_prob': request.session['Question_problem'], 
                	'form': form, 
                	'Action': action, 
                	'results': request.session['Routines'], 
                	'notSelectedRoutines': request.session['notSelectedRoutines'], 
                	'selectedRoutines': request.session['selectedRoutines'],
                	'scriptCode': request.session['userScript'],
			'scriptOutput': request.session['scriptOutput'],
                }
		
		#for item in request.session['selectedRoutines']:
		#	print item
		
		#import pdb; pdb.set_trace()
                return render_to_response(
                	'lighthouse/lapack_le/guided/problem.html', 
                	context_instance=RequestContext(request, context)
                )        

        else:
                context = {
                	'form': ProblemForm(), 
                	'selectedRoutines': request.session['selectedRoutines'],
                	'scriptCode': request.session['userScript'],
			'scriptOutput': request.session['scriptOutput'],
                }
                return render_to_response(
                	'lighthouse/lapack_le/index.html', 
                	context_instance=RequestContext(request, context)
                )




#Question_equation answered.
#Question_complex: Are there complex numbers in your matrix?
def guidedSearch_equation(request):
	form_Equa = EquationForm(request.POST or None)
	if form_Equa.is_valid():
		if form_Equa.cleaned_data['question_equa'] == unicode('transpose'):
			val_0 = 'transpose'
			val_1 = 'A<sup>T</sup>X = B'
			complex_initial_value = 'None'
			request.session['Routines'] = request.session['Routines'].filter(notes__icontains='transpose')

		elif form_Equa.cleaned_data['question_equa'] == unicode('Hermitian_trans'):
			val_0 = 'Hermitian_trans'
			val_1 = 'A<sup>H</sup>X = B'
			complex_initial_value = 'y'	
			request.session['Routines'] = request.session['Routines'].filter(notes__icontains='conjugate transpose')

		else:
			val_0 = 'original'
			val_1 = 'AX = B'
			complex_initial_value = 'None'
			if 'Solve a system of linear equations only' in request.session['Question_problem'][0]:
				request.session['Routines'] = get_model('lighthouse', 'lapack_le_only').objects.filter(notes__icontains='only')
				

		request.session['Question_equation'] = [val_0, val_1] 
		request.session['Complex_initial'] = complex_initial_value
		form = ComplexForm(initial=dict(question_comp=request.session['Complex_initial']))
		filterSelectedRoutines(request)

		context = {
			'query_prob': request.session['Question_problem'], 
			'query_equa': val_1,
			'form': form,
			'results': request.session['Routines'], 
			'notSelectedRoutines': request.session['notSelectedRoutines'], 
			'selectedRoutines': request.session['selectedRoutines'],
			'scriptCode': request.session['userScript'],
			'scriptOutput': request.session['scriptOutput'],
		}					
		return render_to_response(
			'lighthouse/lapack_le/guided/equation.html', 
			context_instance=RequestContext(request, context)
		)

 			
	else:
		form = EquationForm()
		action = '/lighthouse/lapack_le/problem/equation/'
		context = {
			'query_prob': request.session['Question_problem'], 
			'form': form, 
			'Action': action,
			'results': request.session['Routines'], 
			'selectedRoutines': request.session['selectedRoutines'],
			'scriptCode': request.session['userScript'],
			'scriptOutput': request.session['scriptOutput'],
		}
		return render_to_response(
			'lighthouse/lapack_le/guided/problem.html', 
			context_instance=RequestContext(request, context)
		)





##Question_factor answered.
##Question_complex: Are there complex numbers in your matrix?
#def guidedSearch_factor(request):
#	form_Fact = FactorForm(request.POST or None)
#	if form_Fact.is_valid():
#		for val in form_Fact.fields['question_fact'].choices:
#			if val[0] == form_Fact.cleaned_data['question_fact']:
#				request.session['Question_factor'] = [val[0], val[1]]
#				
#		if form_Fact.cleaned_data['question_fact'] == unicode('y'):
#			request.session['FACT'] = 'Y'
#			if 'Solve a system of linear equations only' in request.session['Question_problem'][0]:
#				request.session['Routines'] = request.session['Routines'].filter(notes__icontains='computational')
#			else:
#				request.session['Routines'] = request.session['Routines'].filter(notes__icontains='expert')
#
#		else:
#			request.session['FACT'] = 'N'
#			if 'Solve a system of linear equations only' in request.session['Question_problem'][0] and request.session['Question_equation'][0] == 'original':
#				request.session['Routines'] = request.session['Routines'].filter(notes__icontains='simple')
#			else:
#				request.session['Routines'] = request.session['Routines'].filter(notes__icontains='expert')
#
#		form = ComplexForm(initial=dict(question_comp=request.session['Complex_initial']))
#		filterSelectedRoutines(request)
#
#		context = {
#			'query_prob': request.session['Question_problem'], 
#			'query_equa': request.session['Question_equation'][1],
#			#'query_fact': request.session['Question_factor'][1], 
#			'form': form, 
#			'results': request.session['Routines'], 
#			'notSelectedRoutines': request.session['notSelectedRoutines'],
#			'selectedRoutines': request.session['selectedRoutines'],
#			'scriptCode': request.session['userScript'],
#			'scriptOutput': request.session['scriptOutput'],
#		}					
#		return render_to_response(
#			'lighthouse/lapack_le/guided/factor.html', 
#			context_instance=RequestContext(request, context)
#		)
#
# 			
#	else:
#		form = FactorForm()
#		context = {
#			'query_prob': request.session['Question_problem'], 
#			'query_equa': request.session['Question_equation'][1],
#			'form': form, 
#			'results': request.session['Routines'], 
#			'selectedRoutines': request.session['selectedRoutines'],
#			'scriptCode': request.session['userScript'],
#			'scriptOutput': request.session['scriptOutput'],
#		}
#		return render_to_response(
#			'lighthouse/lapack_le/guided/equation.html', 
#			context_instance=RequestContext(request, context)
#		)






#Question_complex answered.
#Question _matrixType: What is the type of your matrix? 
def guidedSearch_complex(request):
	form_Comp = ComplexForm(request.POST or None)
	if form_Comp.is_valid():
		if form_Comp.cleaned_data['question_comp'] == unicode('n'):
			val_0 = 'n'
			val_1 = 'no'
			request.session['Routines'] = request.session['Routines'].filter(**{'thePrecision__in': ['s', 'd']})
			
		else:
			val_0 = 'y'
			val_1 = 'yes'
			request.session['Routines'] = request.session['Routines'].filter(**{'thePrecision__in': ['c', 'z']})
			
		request.session['Question_complex'] = [val_0, val_1] 
		form = MatrixTypeForm(request)
		filterSelectedRoutines(request)	
		context = {
			'query_prob': request.session['Question_problem'], 
			'query_equa': request.session['Question_equation'][1],
			#'query_fact': request.session['Question_factor'][1], 
			'query_comp': val_1, 
			'form': form,
			'results': request.session['Routines'], 
			'notSelectedRoutines': request.session['notSelectedRoutines'], 
			'selectedRoutines': request.session['selectedRoutines'],
			'scriptCode': request.session['userScript'],
			'scriptOutput': request.session['scriptOutput'],
		}
		
		return render_to_response(
			'lighthouse/lapack_le/guided/complex.html', 
			context_instance=RequestContext(request, context)
		)

 			
	else:
		form = ComplexForm()
		if request.session['Question_equation']==[0, 0]:
			action = '/lighthouse/lapack_le/problem/complex/'
			context = {
				'query_prob': request.session['Question_problem'], 
				'form': form, 
				'Action': action, 
				'results': request.session['Routines']
			}		
			return render_to_response(
				'lighthouse/lapack_le/guided/problem.html', 
				context_instance=RequestContext(request, context)
			)
		else:
			context = {
				'query_prob': request.session['Question_problem'], 
				'query_equa': request.session['Question_equation'][1],
				#'query_fact': request.session['Question_factor'][1],
				'form': form, 
				'results': request.session['Routines'],
				'selectedRoutines': request.session['selectedRoutines'],
				'scriptCode': request.session['userScript'],
				'scriptOutput': request.session['scriptOutput'],
			}
        	return render_to_response(
        		'lighthouse/lapack_le/guided/equation.html', 
        		context_instance=RequestContext(request, context)
        	)





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
				context = {
					'query_prob': request.session['Question_problem'],  
					'query_equa': request.session['Question_equation'][1],
					#'query_fact': request.session['Question_factor'][1], 
					'query_comp': request.session['Question_complex'][1],
					'query_type': val[1], 
					'form': form, 
					'results': request.session['Routines'], 
					'notSelectedRoutines': request.session['notSelectedRoutines'],
					'selectedRoutines': request.session['selectedRoutines'],
					'scriptCode': request.session['userScript'],
					'scriptOutput': request.session['scriptOutput'],
				}
				return render_to_response(
					'lighthouse/lapack_le/guided/matrixtype.html', 
					context_instance=RequestContext(request, context)
				) 

	
	else:
		form = MatrixTypeForm(request)
		context = {
			'query_prob': request.session['Question_problem'], 
			'query_equa': request.session['Question_equation'][1],
			#'query_fact': request.session['Question_factor'][1], 
			'query_comp': request.session['Question_complex'][1],
			'form': form, 
			'results': request.session['Routines'],
			'scriptCode': request.session['userScript'],
			'scriptOutput': request.session['scriptOutput'],
			'selectedRoutines': request.session['selectedRoutines']
		}
		return render_to_response(
			'lighthouse/lapack_le/guided/complex.html', 
			context_instance=RequestContext(request, context)
		)







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
				context = {
					'query_prob': request.session['Question_problem'], 
					'query_equa': request.session['Question_equation'][1],
					#'query_fact': request.session['Question_factor'][1], 
					'query_comp': request.session['Question_complex'][1],
					'query_type': request.session['Question_matrixtype'][1], 
					'query_stor': val[1], 
					'form': form,
					'results': request.session['Routines'], 
					'notSelectedRoutines': request.session['notSelectedRoutines'], 
					'selectedRoutines': request.session['selectedRoutines'],
					'scriptCode': request.session['userScript'],
					'scriptOutput': request.session['scriptOutput'],
				}
				return render_to_response(
					'lighthouse/lapack_le/guided/storagetype.html', 
					context_instance=RequestContext(request, context)
				)

	
	else:
		form = StorageForm(request)
		context = {
			'query_prob': request.session['Question_problem'], 
			'query_equa': request.session['Question_equation'][1],
			#'query_fact': request.session['Question_factor'][1], 
			'query_comp': request.session['Question_complex'][1],
			'query_type': request.session['Question_matrixtype'][1], 
			'form': form,
			'results': request.session['Routines'],
			'scriptCode': request.session['userScript'],
			'scriptOutput': request.session['scriptOutput'],
			'selectedRoutines': request.session['selectedRoutines']
		}
		return render_to_response(
			'lighthouse/lapack_le/guided/matrixtype.html', 
			context_instance=RequestContext(request, context)
		)   






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
		context = {
			'query_prob': request.session['Question_problem'], 
			'query_equa': request.session['Question_equation'][1],
			#'query_fact': request.session['Question_factor'][1], 
			'query_comp': request.session['Question_complex'][1],
			'query_type': request.session['Question_matrixtype'][1], 
			'query_stor': request.session['Question_storagetype'][1],
			'query_prec': val_1, 
			'results': request.session['Routines'], 
			'notSelectedRoutines': request.session['notSelectedRoutines'], 
			'selectedRoutines': request.session['selectedRoutines'],
			'scriptCode': request.session['userScript'],
			'scriptOutput': request.session['scriptOutput'],
		}
		return render_to_response(
			'lighthouse/lapack_le/guided/precision.html', 
			context_instance=RequestContext(request, context)
		)


	else:
		form = PrecisionForm()
		context = {
			'query_prob': request.session['Question_problem'], 
			'query_equa': request.session['Question_equation'][1],
			#'query_fact': request.session['Question_factor'][1], 
			'query_comp': request.session['Question_complex'][1],
			'query_type': request.session['Question_matrixtype'][1], 
			'query_stor': request.session['Question_storagetype'][1],
			'form': form, 'results': request.session['Routines'], 
			'selectedRoutines': request.session['selectedRoutines'],
			'scriptCode': request.session['userScript'],
			'scriptOutput': request.session['scriptOutput'],
		}
		return render_to_response(
			'lighthouse/lapack_le/guided/storagetype.html', 
			context_instance=RequestContext(request, context)
		)











###---------------- Advanced Search ------------------###

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
(1) form = lapack_le_simpleForm() --> form.fields --> {'MatrixType': <django.forms.fields.MultipleChoiceField object at 0x2028590>, 'StorageType': <django.forms.fields.MultipleChoiceField object at 0x2028610>,...}
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
		request.session['scriptOutput']
		request.session['userScript']	
	except (NameError,KeyError):
		request.session['selectedRoutines'] = []
		request.session['scriptOutput'] = ""
		request.session['userScript'] = ""
		
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
				
		context = {
			'Question_advanced': request.session[
			'Question_advanced'], 'Forms': request.session['Forms'], 
			'Function': request.session['Function'], 
			'Complex': request.session['Complex'], 
			'MatrixType':request.session['MatrixType'], 
			'StorageType':request.session['StorageType'], 
			'Precision': request.session['Precision'], 
			'selectedRoutines': request.session['selectedRoutines'],
			'scriptCode': request.session['userScript'],
			'scriptOutput': request.session['scriptOutput'],
		}
		return render_to_response(
			'lighthouse/lapack_le/advanced/advancedForm.html', 
			{'AdvancedTab': True}, 
			context_instance=RequestContext(request, context)
		)

	else:
   		form = AdvancedForm()	
		context = {
			'form': form, 
			'selectedRoutines': request.session['selectedRoutines'],
			'scriptCode': request.session['userScript'],
			'scriptOutput': request.session['scriptOutput'],
		}
		return render_to_response(
			'lighthouse/lapack_le/advanced/advancedSearch.html', 
			{'AdvancedTab': True}, 
			context_instance=RequestContext(request, context)
		)



	


def advancedResult(request):
#----- Display checked items -------#

	for item in ['GETS', 'EquationGETS', 'FunctionGETS', 'ComplexGETS', 'MatrixTypeGETS', 'StorageTypeGETS', 'PrecisionGETS']:  
		request.session[item] = []

	request.session['Results'] = {}
	routines = []

	for model in request.session['App_Model']:
		form_empty = str_to_class(model[1]+'Form')()
		form = str_to_class(model[1]+'Form')(request.POST or None)
		if model[1] == 'lapack_le_expert':
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
			if model[1] == 'lapack_le_expert':
				selected_Equation = form.cleaned_data[model[1]+'Equation']
				for comp in selected_Complex:
					for precision in selected_Precision:
						for matrix in selected_MatrixType:
							for storage in selected_StorageType:
								for function in selected_Function:
									for equation in selected_Equation:
										#print equation
										#print form_empty.find_equation(equation)
										if equation == 'solve':
											routine = lapack_le_expert.objects.filter(
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
 											routine = lapack_le_expert.objects.filter(
 												thePrecision=whatPrecision(comp, precision), 
 												matrixType=matrix, storageType=storage, 
 												notes__icontains=equation).filter(notes__icontains=function
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
									routine = get_model('lighthouse', model[1]).objects.filter(
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
										'Equation': '', 
										'Description': form.Description,
										'Routine': routine
									})
									routines.append(list(routine))
		
	alreadySelectedRoutines = filterSelectedRoutines3(request, routines)
	#L = []
	context = {
		'Question_advanced': request.session['Question_advanced'], 
		'GETS': request.session['GETS'], 
		'FunctionGETS': request.session['FunctionGETS'],
		'ComplexGETS': request.session['ComplexGETS'], 
		'MatrixTypeGETS':request.session['MatrixTypeGETS'], 
		'StorageTypeGETS':request.session['StorageTypeGETS'],
		'PrecisionGETS': request.session['PrecisionGETS'], 
		'EquationGETS': request.session['EquationGETS'], 
		'selected_Equation': selected_Equation, 
		'Results': request.session['Results'], 
		'alreadySelectedRoutines': alreadySelectedRoutines, 
		'selectedRoutines': request.session['selectedRoutines'],
		'scriptCode': request.session['userScript'],
		'scriptOutput': request.session['scriptOutput'],
	}	

	return render_to_response(
		'lighthouse/lapack_le/advanced/advancedResult.html', 
		{'AdvancedTab': True}, 
		context_instance=RequestContext(request, context)
	)

#	else:
#   		form = AdvancedForm()	
#    		context = {'form': form}

#    		return render_to_response('lighthouse/lapack_le/advanced/advanced_search.html', context_instance=RequestContext(request, context))






###---------------- Keyword Search ------------------###
special_words = {
		'dataType': ['real', 'complex'],
		'thePrecision': ['single', 'double'],
		'matrixType': ['general', 'symmetric', 'Hermitian', 'SPD', 'HPD', 'symmetric positive definite', 'Hermitian positive definite', 'triangular', 'SPsD', 'HPsD', 'symmetric positive semidefinite', 'Hermitian positive semidefinite'],
		'storageType': ['full', 'band', 'packed', 'tridiagonal', 'rectangular full packed', 'RFP'],
		'table': ['factor', 'factorization', 'condition number', 'error bound', 'equilibrate', 'inverse', 'driver', 'expert', 'computational', 'solve', 'refine',],
	}


def keywordResult(request):
	modelList = []
	answer = []
	keywords_dictionary = {}
	keywords_origList = []
	keywordsList = []
	keywords = ""

	try:
		request.session['selectedRoutines']
		request.session['scriptOutput']
		request.session['userScript']	
	except (NameError,KeyError):
		request.session['selectedRoutines'] = []
		request.session['scriptOutput'] = ""
		request.session['userScript'] = ""
	
	if request.method == 'POST':		
		form = ModelSearchForm(request.POST)
		#print form
		
		if form.is_valid():
			## if driver/computational boxes are checked
			#answer_class = form.cleaned_data['models']
				
			## get the keyword
			keywords_orig = request.POST['q']
			
			## Don't split double-quoted words ##
			keywords_origList = shlex.split(keywords_orig)
			
			## split all words ##
			keywords_singleList = keywords_orig.split()
			
			## spell check ##
			for i, item in enumerate(keywords_singleList):
				keywords_singleList[i] = spell_check(item)	
				
			## make a string out of keywordsList ##
			keywords = " ".join(keywords_singleList)
			
			## keywords goes through keyword_handler ##
			keywords = keyword_handler(keywords)
			#print keywords
			
			## final keywordsList, Don't split double-quoted words
			keywordsList = shlex.split(keywords)
			
			## find the words that are not corrected ##
			common = list(set(keywords_origList) & set(keywordsList))
			#print common
			
			
			###***** make a dictionary for the keywords for django query *****###
			sumList = []
			for key in special_words:
				keywords_dictionary[key] = list(set(keywordsList) & set(special_words[key]))
				sumList += keywords_dictionary[key]
			keywords_dictionary['other'] = list(set(keywordsList) - set(sumList))
			
			## keep 'transpose' and 'conjugate transpose' only
			keywords_dictionary['other'] = list(set(['transpose', 'conjugate transpose']) & set(keywords_dictionary['other']))
			print keywords_dictionary
			
			if not any([keywords_dictionary[i] == [] for i in ['table', 'matrixType']]):
				print 'use django'
				keywords_dictionary = keyword_handler2(keywords_dictionary)
				keywords_dictionary = kwDictionary_set(keywords_dictionary)
				#print keywords_dictionary
				results = query_django(keywords_dictionary)				
			else:
				print 'use haystack'
				results = SearchQuerySet().models(lapack_le_driver, lapack_le_computational).filter(content=AutoQuery(keywords)).order_by('id')
							
			
			context = {'results': results,
				   'keywordsList': keywordsList,
				   'common': common,
				   'selectedRoutines': request.session['selectedRoutines'],
				   #'notSelectedRoutines': request.session['notSelectedRoutines'],
				   }
			
			return render_to_response(
				'lighthouse/lapack_le/keywordResult.html', 
				{'KeywordTab': True}, 
				context_instance=RequestContext(request, context)
			)
		else:
			HttpResponse("Error!")
			
			
			

#def quoted_words(string):
#	matches=re.findall(r'\"(.+?)\"', string)
#	return matches
	


def spell_check(word):
	word = re.sub(r'\b.*?qu.*?lib.*?\b', 'equilibrate', word)
	if word.lower() in ['spd', 'hpd', 'lu', 'qr', 'rfp']:
		word = word.upper()
	elif word.lower() == 'spsd':
		word = 'SPsD'
	elif word.lower() == 'hpsd':
		word = 'HPsD'		
	else: 	
		word= correct(word)
	return word


	
def keyword_handler(strings):
	strings = re.sub(r'\bli.*? eq.*?\b', 'linear equations', strings)
	strings = re.sub(r'\blinear equations s.*?ver\b', 'linear equations solver', strings)
	strings = re.sub(r'\bsys.*? linear eq.*?\b', 'system of linear equations', strings)
	strings = re.sub(r'\berror b.*?\b', '\"'+'error bounds'+'\"', strings)
	strings = re.sub(r'\bcondition n.*?\b', '\"'+'condition number'+'\"', strings)
	strings = re.sub(r'\bLU fact.*?\b', '\"'+'LU factorization'+'\"', strings)
	strings = re.sub(r'\bCh.*?ky fact.*?\b', '\"'+'Cholesky factorization'+'\"', strings)
	strings = re.sub(r'\bLU decomp.*?\b', '\"'+'LU decomposition'+'\"', strings)
	strings = re.sub(r'\bequilib.*?\b', 'equilibrate', strings)
	#strings = re.sub(r'\binv.*?t.*?\b', 'invert', strings)
	strings = re.sub(r'\binvert*?\b', 'inverse', strings)
	strings = re.sub(r'\bhermitian.*?\b', 'Hermitian', strings)
	strings = re.sub(r'\bHermitian p.*? def.*?\b', '\"'+'Hermitian positive definite'+'\"', strings)
	strings = re.sub(r'\bHermitian p.*? semidef.*?\b', '\"'+'Hermitian positive semidefinite'+'\"', strings)
	strings = re.sub(r'\bsymmetric.*?\b', 'symmetric', strings)
	strings = re.sub(r'\bsymmetric p.*? def.*?\b', '\"'+'symmetric positive definite'+'\"', strings)
	strings = re.sub(r'\bsymmetric p.*? semidef.*?\b', '\"'+'symmetric positive semidefinite'+'\"', strings)
	strings = re.sub(r'\bband.*?\b', 'band', strings)
	strings = re.sub(r'\bpack.*?\b', 'packed', strings)
	strings = re.sub(r'\brectang.*?\b fu.*? pa.*?\b', '\"'+'rectangular full packed'+'\"', strings)
	strings = re.sub(r'\gauss.*? elim.*?\b', 'Gaussian elimination', strings)
	strings = re.sub(r'\conj.*? tra.*?\b', '\"'+'conjugate transpose'+'\"', strings)
	return strings	






###------- set up keywords_dictionary to use proper model names--------###
table_handler2 = {
	'factorization': 'factor',
	'condition number': 'condition_number',
	'error bound': 'error_bound',
	'refine': 'error_bound',
	'refinement': 'error_bound',
}

matrixType_handler2 = {
	'symmetric positive definite': 'SPD',
	'Hermitian positive definite': 'HPD',
	'symmetric positive semidefinite': 'SPsD',
	'Hermitian positive semidefinite': 'HPsD',
}
def keyword_handler2(keywords_dictionary):
	## change table name
	for i, item in enumerate(keywords_dictionary['table']):
		for key, value in table_handler2.iteritems():
			if item == key:
				keywords_dictionary['table'][i] = value
				
	## change matrix type name
	for i, item in enumerate(keywords_dictionary['matrixType']):
		for key, value in matrixType_handler2.iteritems():
			if item == key:
				keywords_dictionary['matrixType'][i] = value

	## change storage type name
	for i, item in enumerate(keywords_dictionary['storageType']):
		if item == 'rectangular full packed':
			keywords_dictionary['storageType'][i] = 'RFP'

				
	## for 'solve a system of linear equations'
	if len(keywords_dictionary['table']) == 1 and 'solve' in keywords_dictionary['table']:
		keywords_dictionary['table'] = ['only', 'expert']
		
	elif 'driver' in (keywords_dictionary['table']) and 'solve' in keywords_dictionary['table']:
		keywords_dictionary['table'] = ['driver']
		
	elif 'computational' in (keywords_dictionary['table']) and 'solve' in keywords_dictionary['table']:
		keywords_dictionary['table'] = ['solve']
		
	elif len(keywords_dictionary['table'])>1 and 'solve' in keywords_dictionary['table']:
		keywords_dictionary['table'] = ['expert']
		
	else:
		pass	
	
	
	return keywords_dictionary	





###------- set up keywords_dictionary for query_django--------###
def kwDictionary_set(keywords_dictionary):		
	###***** convert table strings to class *****###
	for i,value in enumerate(keywords_dictionary['table']):	
		value = "lapack_le_"+value
		tableClass = ContentType.objects.get(model=value).model_class()
		keywords_dictionary['table'][i] = tableClass
		
	if keywords_dictionary['storageType']==[]:
		keywords_dictionary['storageType'] = ['full']
		
	###***** combine matrixType and storageType *****###
	keywords_dictionary.update({'matrix_storage':[]})
	for matrix in keywords_dictionary['matrixType']:
		for storage in keywords_dictionary['storageType']:
			combineType = matrix+"_"+storage
			keywords_dictionary['matrix_storage'].append(combineType)
	
	###***** delete keywords_dictionary['matrixType'] and keywords_dictionary['storageType'] *****###
	del keywords_dictionary['matrixType']
	del keywords_dictionary['storageType']
	
	
	###***** combine dataType and thePrecision to determine 's', 'd', 'c', 'z'. *****###
	precisionList = []
	if len(keywords_dictionary['dataType']) == 0 and len(keywords_dictionary['thePrecision']) != 0:
		for precision in keywords_dictionary['thePrecision']:
			if precision =='single':
				precisionList.extend(['s', 'c'])
			else:
				precisionList.extend(['d', 'z'])
	elif len(keywords_dictionary['dataType']) != 0 and len(keywords_dictionary['thePrecision']) == 0:
		for data in keywords_dictionary['dataType']:
			if data == 'real':
				precisionList.extend(['s', 'd'])
			else:
				precisionList.extend(['c', 'z'])
	elif len(keywords_dictionary['dataType']) == 0 and len(keywords_dictionary['thePrecision']) == 0:
		precisionList.extend(['s', 'd', 'c', 'z'])
	else:
		for data in keywords_dictionary['dataType']:
			for precision in keywords_dictionary['thePrecision']:
				if data == 'real' and precision =='single':
					precision = 's'
				elif data == 'real' and precision =='double':
					precision = 'd'
				elif data == 'complex' and precision =='single':
					precision = 'c'
				else:
					precision = 'z'
				precisionList.append(precision)
	keywords_dictionary['thePrecision'] = precisionList
			
	###***** delete keywords_dictionary['dataType'] *****###
	del keywords_dictionary['dataType']
	return keywords_dictionary





###-------- Django querry search --------###
def query_django(keywords_dictionary):
	results = []
	for table in keywords_dictionary['table']:
		#print table
		for combineType in keywords_dictionary['matrix_storage']:
			for precision in keywords_dictionary['thePrecision']:
				kwargs = {'matrixType': combineType.split('_')[0],
					'storageType': combineType.split('_')[1],
					'thePrecision': precision}
				if keywords_dictionary['other'] == []:
					results += table.objects.filter(**kwargs).order_by('id')
				else:
					for item in keywords_dictionary['other']:
						results += table.objects.filter(**kwargs).filter(notes__icontains=item).order_by('id')
	return results










###---------------- for Selected and NotSelected----------------###
"""
From the list of routines returned after each step of the Guided Search (i.e. request.session['Routines']), 
this function creates a new list of routines that excludes the routines 
that are in the request.session['selectedRoutines'] list
"""
def filterSelectedRoutines(request):	
	request.session['notSelectedRoutines'] = request.session['Routines']

	for item in request.session['selectedRoutines']:
		request.session['notSelectedRoutines'] = request.session['notSelectedRoutines'].exclude(Q(thePrecision=item['thePrecision']), Q(routineName=item['routineName']))	
	
	request.session.modified = True




"""
From the list of routines returned by a Keyword Search this function removes 
the routines that are in the request.session['selectedRoutines'] list
"""
def filterSelectedRoutines2(request, routines): 
	indices = []
	i = 0
	for item1 in routines:		
		for item2 in request.session['selectedRoutines']:
			# Save the indices of the routines that match
			if item2['thePrecision'] == item1.thePrecision and item2['routineName'] == item1.routineName:
				indices.append(i)
		i += 1

	# Reverse the list, so the routine with highest index gets popped first 
	# (popping the lowest index first messes up the list)
	indices.reverse()

	for item in indices:
		routines.pop(item)
	
	return routines



"""
From the list of routines returned by an Advanced Search this function  
creates a new list of routines (in the same format as the search result)
that contains only the routines that are in request.session['selectedRoutines'] list
"""
def filterSelectedRoutines3(request, routines):
	alreadySelectedRoutines = []

	for item1 in request.session['selectedRoutines']:
		for lst in routines:
			for item2 in lst:
				if item2.thePrecision == item1['thePrecision'] and item2.routineName == item1['routineName']:
					alreadySelectedRoutines.append(item2)

	return alreadySelectedRoutines







###---------------- Script ------------------###

@csrf_exempt
def runScript(request):
	
	code = request.POST.get('scriptCode')

	if code == "":
		request.session['userScript'] = ""
		request.session['scriptOutput'] = ""
		output = ""
	else:
		bto = BTOGenerator()
		output = bto.generateCode(str(code))		
		request.session['userScript'] = code
		request.session['scriptOutput'] = output

	request.session.modified = True

	return HttpResponse(output)






###---------------- Ajax post to update request.session['selectedRoutines']------------------###
@csrf_exempt
def update_session(request):
	if request.is_ajax():
		selectedRoutineNames = []
		selectedRoutineList = [{
			"thePrecision": request.POST.get('precision'),
			"routineName": request.POST.get('routineName'),
			"matrixType": request.POST.get('matrixType'),
			"storageType": request.POST.get('storageType'),
			"id": request.POST.get('idn'),
			"url": request.POST.get('url'),
			#"checkState": request.POST.get('checkState')
		}]
		
		if selectedRoutineList[0] not in request.session['selectedRoutines']:
			request.session['selectedRoutines'] = request.session['selectedRoutines'] + selectedRoutineList

		return HttpResponse('Dropped '+request.POST.get('precision')+request.POST.get('routineName'))
		
		## Check if the routine already exists in request.session['selectedRoutines'], if it does save it's index
		#counter = 0
		#match = -1
		#for item in request.session['selectedRoutines']:
		#	if item['thePrecision'] == selectedRoutineList[0]['thePrecision'] and item['routineName'] == selectedRoutineList[0]['routineName']:
		#		match = counter # Save the index
		#		if selectedRoutineList[0]['checkState'] == 'checked':
		#			request.session['selectedRoutines'][counter]['checkState'] = 'checked'
		#		if selectedRoutineList[0]['checkState'] == 'unchecked':
		#			request.session['selectedRoutines'][counter]['checkState'] = 'unchecked'							
		#	counter += 1
		#
		#if match == -1: # The routine does not exist in request.session['selectedRoutines'], so add it
		#	request.session['selectedRoutines'] = request.session['selectedRoutines'] + selectedRoutineList
		#
		## Session was modified
		#request.session.modified = True
		#
		## Create a list of all checked routines	
		#for item in request.session['selectedRoutines']:
		#	if item['checkState'] == 'checked':
		#		selectedRoutineNames.append(item['thePrecision']+item['routineName']+',')
		#
		## Return the list
		#return HttpResponse(selectedRoutineNames)
	else:
		return HttpResponse('only AJAX requests are allowed!')






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
		rouitnePrecision = mode[0]['routine'][0]
		routineName = mode[0]['routine'][1:]
		for i, item in enumerate(request.session['selectedRoutines']):
			if item.get('routineName') == routineName and item.get('thePrecision') == rouitnePrecision:
				del request.session['selectedRoutines'][i]
				
		### important: mark the session as modified for it to save		
		request.session.modified = True

		return HttpResponse('Removed '+rouitnePrecision+routineName)		
	else:
		return HttpResponse('only AJAX requests are allowed!')
