import string, types, sys
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





###---------------- Home Page ------------------###
#Question_problem: Which of the following functions do you wish to execute?
#@login_required
def search_forms(request):
    request.session.clear()	
    context = {'form': ProblemForm(), 'formAdvanced': AdvancedForm(), 'scriptForm': scriptForm()}
    return render_to_response('Search/index.html', context_instance=RequestContext(request, context))





#Question_problem answered!
#Question_equation: What form of the linear system do you want to solve? 
#or 
#Question_complex: Are there complex numbers in your matrix?
@csrf_exempt
def guidedSearch_problem(request):
        form_Prob = ProblemForm(request.POST or None)
        request.session['Question_problem'] = []
        request.session['queries'] = []
        request.session['mykey'] = 'ok'

        if form_Prob.is_valid():
                selected = form_Prob.cleaned_data['question_prob']
                for answer in selected:
                        request.session['Question_problem'].append((answer, ProblemForm().find(answer))) 

                appName = selected[0].split()[0]
                modelName = selected[0].split()[1]
                for item in selected:
                        request.session['queries'].append(Q(notes__icontains=item.split()[2]))

                request.session['Routines'] = get_model(appName,modelName).objects.filter(combine_Q(request.session['queries']))        

                if appName == 'Driver' or appName == 'Combine':
                        form = EquationForm()
                        action = '/search/guided/problem_equation/'
        
                else:
                        form = ComplexForm()
                        action = '/search/guided/problem_complex/'
                        request.session['Question_equation']=[0, 0]
                        request.session['Question_factor']=[0, 0]       

                context = {'query_prob': request.session['Question_problem'], 'form': form, 'Action': action, 'results': request.session['Routines']}
                return render_to_response('Search/problem.html', context_instance=RequestContext(request, context))        

        else:
                context = {'form': ProblemForm()}
                return render_to_response('Search/index.html', context_instance=RequestContext(request, context))






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
		

		context = {'query_prob': request.session['Question_problem'], 'query_equa': val_1, 'form': form, 'results': request.session['Routines']}					
		return render_to_response('Search/equation.html', context_instance=RequestContext(request, context))

 			
	else:
        	form = EquationForm()
		action = '/search/problem/equation/'
		context = {'query_prob': request.session['Question_problem'], 'form': form, 'Action': action, 'results': request.session['Routines']}
        	return render_to_response('Search/problem.html', context_instance=RequestContext(request, context))





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

		context = {'query_prob': request.session['Question_problem'], 'query_equa': request.session['Question_equation'][1], 'query_fact': request.session['Question_factor'][1], 'form': form, 'results': request.session['Routines']}					
		return render_to_response('Search/factor.html', context_instance=RequestContext(request, context))

 			
	else:
        	form = FactorForm()
		context = {'query_prob': request.session['Question_problem'], 'query_equa': request.session['Question_equation'][1],'form': form, 'results': request.session['Routines']}
        	return render_to_response('Search/equation.html', context_instance=RequestContext(request, context))






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
		request.session['selectedRoutines'] = [{"thePrecision": "S", "routineName": "GETRI", "matrixType": "general", "storageType": "full", "id": "197", "url": "http://www.netlib.org/lapack/single/sgetri.f"},
			{"thePrecision": "D", "routineName": "GETRI", "matrixType": "general", "storageType": "full", "id": "198", "url": "http://www.netlib.org/lapack/double/dgetri.f"}]
		context = {'query_prob': request.session['Question_problem'], 'query_equa': request.session['Question_equation'][1],
			   'query_fact': request.session['Question_factor'][1], 'query_comp': val_1, 'form': form,
			   'results': request.session['Routines'], 'test': request.session['selectedRoutines']}					
		
		return render_to_response('Search/complex.html', context_instance=RequestContext(request, context))

 			
	else:
        	form = ComplexForm()
		if request.session['Question_equation']==[0, 0]:
			action = '/search/problem/complex/'
			context = {'query_prob': request.session['Question_problem'], 'form': form, 'Action': action, 'results': request.session['Routines']}		
			return render_to_response('Search/problem.html', context_instance=RequestContext(request, context))
	
		else:
			context = {'query_prob': request.session['Question_problem'], 'query_equa': request.session['Question_equation'][1], 'query_fact': request.session['Question_factor'][1],'form': form, 'results': request.session['Routines']}
        		return render_to_response('Search/factor.html', context_instance=RequestContext(request, context))





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
				context = {'query_prob': request.session['Question_problem'],  'query_equa': request.session['Question_equation'][1], 'query_fact': request.session['Question_factor'][1], 'query_comp': request.session['Question_complex'][1], 'query_type': val[1], 'form': form, 'results': request.session['Routines']}
				return render_to_response('Search/matrixtype.html', context_instance=RequestContext(request, context)) 

	
	else:
		form = MatrixTypeForm(request)
		context = {'query_prob': request.session['Question_problem'], 'query_equa': request.session['Question_equation'][1], 'query_fact': request.session['Question_factor'][1], 'query_comp': request.session['Question_complex'][1], 'form': form, 'results': request.session['Routines']}
		return render_to_response('Search/complex.html', context_instance=RequestContext(request, context))







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
				context = {'query_prob': request.session['Question_problem'], 'query_equa': request.session['Question_equation'][1], 'query_fact': request.session['Question_factor'][1], 'query_comp': request.session['Question_complex'][1], 'query_type': request.session['Question_matrixtype'][1], 'query_stor': val[1], 'form': form, 'results': request.session['Routines']}
				return render_to_response('Search/storagetype.html', context_instance=RequestContext(request, context)) 

	
	else:
		form = StorageForm(request)
		context = {'query_prob': request.session['Question_problem'], 'query_equa': request.session['Question_equation'][1], 'query_fact': request.session['Question_factor'][1], 'query_comp': request.session['Question_complex'][1], 'query_type': request.session['Question_matrixtype'][1], 'form': form, 'results': request.session['Routines']}
		return render_to_response('Search/matrixtype.html', context_instance=RequestContext(request, context))   






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

		context = {'query_prob': request.session['Question_problem'], 'query_equa': request.session['Question_equation'][1], 'query_fact': request.session['Question_factor'][1], 'query_comp': request.session['Question_complex'][1], 'query_type': request.session['Question_matrixtype'][1], 'query_stor': request.session['Question_storagetype'][1], 'query_prec': val_1, 'results': request.session['Routines']}
		return render_to_response('Search/precision.html', context_instance=RequestContext(request, context)) 		


	else:
		form = PrecisionForm()
		context = {'query_prob': request.session['Question_problem'], 'query_equa': request.session['Question_equation'][1], 'query_fact': request.session['Question_factor'][1], 'query_comp': request.session['Question_complex'][1], 'query_type': request.session['Question_matrixtype'][1], 'query_stor': request.session['Question_storagetype'][1], 'form': form, 'results': request.session['Routines']}
		return render_to_response('Search/storagetype.html', context_instance=RequestContext(request, context))











###---------------- Advanced Search ------------------###
#def advancedSearch(request):
#    request.session.clear()
#
#    form = AdvancedForm()	
#    context = {'form': form}
#
#    return render_to_response('Search/advancedSearch.html', context_instance=RequestContext(request, context))



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
	AdvancedTab = True

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
				
		context = {'Question_advanced': request.session['Question_advanced'], 'Forms': request.session['Forms'], 'Function': request.session['Function'], 'Complex': request.session['Complex'], 'MatrixType':request.session['MatrixType'], 'StorageType':request.session['StorageType'], 'Precision': request.session['Precision']}
		return render_to_response('Search/advancedForm.html', {'AdvancedTab': AdvancedTab}, context_instance=RequestContext(request, context))

	else:
   		form = AdvancedForm()	
		context = {'form': form}
		return render_to_response('Search/advancedSearch.html', {'AdvancedTab': AdvancedTab}, context_instance=RequestContext(request, context))



	


def advancedResult(request):
#----- Display checked items -------#
	for item in ['GETS', 'EquationGETS', 'FunctionGETS', 'ComplexGETS', 'MatrixTypeGETS', 'StorageTypeGETS', 'PrecisionGETS']:  
		request.session[item] = []

	request.session['Results'] = {}

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
										if equation=='solve':
											request.session['Results'][model[1]].append({'Complex number': comp, 'Precision': precision, 'Matrix type': matrix,
																     'Storage type': storage, 'Function': form_empty.find_function(function),
																     'Equation': form_empty.find_equation(equation), 'Description': form.Description,
																     'Routine': LinearEquation_expert.objects.filter(thePrecision=whatPrecision(comp, precision), matrixType=matrix, storageType=storage, notes__icontains=function)})
 										else: 
											request.session['Results'][model[1]].append({'Complex number': comp, 'Precision': precision, 'Matrix type': matrix,
																     'Storage type': storage, 'Function': form_empty.find_function(function),
																     'Equation': form_empty.find_equation(equation), 'Description': form.Description,
																     'Routine': LinearEquation_expert.objects.filter(thePrecision=whatPrecision(comp, precision), matrixType=matrix, storageType=storage, notes__icontains='trans').filter(notes__icontains=function)})

			else:
				for comp in selected_Complex:
					for precision in selected_Precision:
						for matrix in selected_MatrixType:
							for storage in selected_StorageType:
								for function in selected_Function:
									request.session['Results'][model[1]].append({'Complex number': comp, 'Precision': precision, 'Matrix type': matrix,
														     'Storage type': storage, 'Function': form_empty.find(function),
														     'Equation': 0, 'Description': form.Description,
														     'Routine': get_model(*model).objects.filter(thePrecision=whatPrecision(comp, precision), matrixType=matrix, storageType=storage, notes__icontains=function)})						
		
	
	AdvancedTab = True		
	context = {'Question_advanced': request.session['Question_advanced'], 'GETS': request.session['GETS'], 'FunctionGETS': request.session['FunctionGETS'],
		   'ComplexGETS': request.session['ComplexGETS'], 'MatrixTypeGETS':request.session['MatrixTypeGETS'], 'StorageTypeGETS':request.session['StorageTypeGETS'],
		   'PrecisionGETS': request.session['PrecisionGETS'], 'EquationGETS': request.session['EquationGETS'], 'selected_Equation': selected_Equation, 'Results': request.session['Results']}
	return render_to_response('Search/advancedResult.html', {'AdvancedTab': AdvancedTab}, context_instance=RequestContext(request, context))

#	else:
#   		form = AdvancedForm()	
#    		context = {'form': form}

#    		return render_to_response('Search/advanced_search.html', context_instance=RequestContext(request, context))











'''

###---------------- General Search ------------------###
def search_result(request):
    error = False
    if 'q' in request.POST:
        Q = request.POST['q']
	q = Q.lower()
        if not q:
            error = True
        else:
            	routines = []
		routines_le_simple = SearchQuerySet().models(LinearEquation_simple).filter(info__istartswith=q)
		routines_le_expert = SearchQuerySet().models(LinearEquation_expert).filter(info__istartswith=q)
            	routines_le_computational = SearchQuerySet().models(LinearEquation_computational).filter(info__istartswith=q)
		routines = list(chain(routines_le_simple, routines_le_expert, routines_le_computational))

		return render_to_response('Search/search_results.html', {'results': routines, 'query': Q})

   
    return render_to_response('Search/search_form.html', {'error': error})


'''



###---------------- Script ------------------###
def runScript(request):
  if request.method == 'POST':
    form = scriptForm(request.POST)
    if form.is_valid():
      request.session['userScript'] = form.cleaned_data['script']
      #request.session['generatedCodeTemplate'] = "User input: " + request.session['userScript']
      bto = BTOGenerator()
      request.session['generatedCodeTemplate'] = bto.generateCode(str(request.session['userScript']))
      result = request.session['generatedCodeTemplate']
      context = {'form': ProblemForm(), 'formAdvanced': AdvancedForm(), 
        'scriptForm': scriptForm(initial={'script': request.session['userScript']}), 
        'generatedCodeTemplate': request.session['generatedCodeTemplate']}
      return render_to_response('Search/index.html', context_instance=RequestContext(request, context))

    else:
      context = {'form': ProblemForm(), 'formAdvanced': AdvancedForm(), 'scriptForm': scriptForm()}
      return render_to_response('Search/index.html', context_instance=RequestContext(request, context))




def update_session(request):
    if not request.is_ajax() or not request.method=='POST':
        return HttpResponseNotAllowed(['POST'])
    
    else:
        #request.session['mykey'] = json.loads(request.POST.get('details', '[]'))
        #return HttpResponse(request.session['mykey'])
        request.session['mykey'] = 'not ok'
        return request.session['mykey']

