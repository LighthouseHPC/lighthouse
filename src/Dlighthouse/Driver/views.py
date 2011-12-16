import string
from Driver.models import RoutineInfo, LinearEquation_simple, LinearEquation_expert 
from Computational.models import LinearEquation_computational
from django.http import HttpResponse, HttpResponseRedirect
from django.shortcuts import render_to_response
from django.contrib.contenttypes.models import ContentType
from django.db.models import get_model, Q
from itertools import chain
from haystack.query import SearchQuerySet
from Driver.forms import ProblemForm, EquationForm, FactorForm, PrecisionForm, ComplexForm, MatrixTypeForm, StorageForm, AdvancedForm
from django.template import RequestContext


#Combine (and) the Q's.
def combine_Q(aList):
	query = Q()
	for value in aList:
		query &= value
	return query	




#Question 1: What is the type of the problem you would like to solve?

def search_form(request):
    request.session.clear()	
    return render_to_response('search_form.html')




#"Problem" question answered. ---> Question 1.5: What form of the linear system do you want to solve? OR Question 2: Are there complex numbers in your matrix? 

#MODEL.objects.filter(**condition), where condition must be a dictionary.
#"request.session" is a dictionary that records the answers: request.session = {Routines: [], 'Question_problem': [], 'Question_equation': [], ...}
#get_model('appName', 'modelName')
#form.cleaned_data and request.GET.getlist do the same thing --> list the chosen options.
#Model_List=[]  ---> when calling more than one models for SearchQuerySet().
def search_problem(request):
	form_Prob = ProblemForm(request.GET or None)
	Answers = []
	Routines = {}
	queries = []
	if form_Prob.is_valid():
		selected = form_Prob.cleaned_data['question_prob']
		for answer in selected:
			Answers.append((answer, ProblemForm().find(answer))) 

		appName = selected[0].split()[0]
		modelName = selected[0].split()[1]
		for item in selected:
			queries.append(Q(notes__icontains=item.split()[2]))

		if selected[0].split()[2] == 'solve':
			routines = list(chain(LinearEquation_simple.objects.all(), LinearEquation_expert.objects.filter(notes__icontains='trans'), LinearEquation_computational.objects.filter(notes__icontains='solve')))

		else:
			routines = get_model(appName, modelName).objects.filter(combine_Q(queries))	

		if appName == 'Driver':
			form = EquationForm()
			action = '/search/problem/equation/'
	
		else:
			form = ComplexForm()
			action = '/search/problem/complex/'


		request.session['Question_problem'] = Answers		
		request.session['Routines'] = routines	

		context = {'query_prob': Answers, 'form': form, 'Action': action, 'results': routines}
		return render_to_response('problem.html', context_instance=RequestContext(request, context))
			

	else:
		form_Prob = ProblemForm()
		context = {'form': form_Prob}
		return render_to_response('search_form.html', context_instance=RequestContext(request, context))





#"Equation" question answered. ---> Question 2: Are there complex numbers in your matrix?
def search_equation(request):
	form_Equa = EquationForm(request.GET or None)
	if form_Equa.is_valid():
		if form_Equa.cleaned_data['question_equa'] == unicode('transpose'):
			val_0 = 'transpose'
			val_1 = 'A**T*X = B'
			initial_value = 'None'
			for item in request.session['Question_problem']:
				if 'LinearEquation' in item:
					routines = list(chain(LinearEquation_computational.objects.filter(Q(thePrecision='s')|Q(thePrecision='d')), LinearEquation_expert.objects.filter.Q(thePrecision='s')|Q(thePrecision='d')))

				elif 'equilibrate' in item:
					routines = LinearEquation_expert.objects.filter(notes__icontains='trans').filter(Q(thePrecision='s')|Q(thePrecision='d'))
				else:
					routines = LinearEquation_expert.objects.all().filter(Q(thePrecision='s')|Q(thePrecision='d'))


		elif form_Equa.cleaned_data['question_equa'] == unicode('Hermitian_trans'):
			val_0 = 'Hermitian_trans'
			val_1 = 'A**H*X = B'
			initial_value = 'y'	
			for item in request.session['Question_problem']:
				if 'equilibrate' in item:
					routines = LinearEquation_expert.objects.filter(notes__icontains='trans').filter(Q(thePrecision='c')|Q(thePrecision='z'))
				else:
					routines = LinearEquation_expert.objects.all().filter(Q(thePrecision='c')|Q(thePrecision='z'))

		else:
			val_0 = 'original'
			val_1 = 'A*X = B'
			initial_value = 'None'
			for item in request.session['Question_problem']:
				if 'LinearEquation' in item:
					routines = list(chain(LinearEquation_simple.objects.all(), 
							LinearEquation_computational.objects.filter(notes__icontains='solve')))			

		request.session['Routines'] = routines
		request.session['Question_problem.5'] = [val_0, val_1] 
		form = FactorForm(initial=dict(question_fact=initial_value))

		context = {'query_prob': request.session['Question_problem'], 'query_equa': val_1, 'form': form, 'results': routines}					
		
		return render_to_response('equation.html', context_instance=RequestContext(request, context))

 			
	else:
        	form = EquationForm()
		context = {'query_prob': request.session['Question_problem'], 'form': form, 'results': request.session['Routines']}
        	return render_to_response('problem.html', context_instance=RequestContext(request, context))





def search_factor(request):
	form_Fact = FactorForm(request.GET or None)
	if form_Fact.is_valid():
		for val in form_Fact.fields['question_fact'].choices:
			if val[0] == form_Type.cleaned_data['question_fact']:
				request.session['Question_problem.7'] = [val[0], val[1]]

		if form_Fact.cleaned_data['question_fact'] == unicode('y'):
			for item in request.session['Question_problem']:
				if 'LinearEquation' in item:
					routines = LinearEquation_computational.objects.filter(notes__icontains='solve')
				else:
					routines = LinearEquation_expert.objects.filter(notes__icontains='solve')








#"Complex" question answered. ---> Question 3: What is the type of your matrix? 

def search_complex(request):
	form_Comp = ComplexForm(request.GET or None)
	if form_Comp.is_valid():
		if form_Comp.cleaned_data['question_comp'] == unicode('y'):
			val_0 = 'y'
			val_1 = 'Yes'
			request.session['Routines'] = request.session['Routines'].filter(**{'thePrecision__in': ['c', 'z']})

			
		else:
			val_0 = 'n'
			val_1 = 'No'
			request.session['Routines'] = request.session['Routines'].filter(**{'thePrecision__in': ['s', 'd']})
			
		request.session['Question2'] = [val_0, val_1] 
		form = MatrixTypeForm(request)	
		context = {'query_prob': request.session['Question_problem'], 'query_comp': val_1, 'form': form, 'results': request.session['Routines']}					
		
		return render_to_response('complex.html', context_instance=RequestContext(request, context))

 			
	else:
        	form = ComplexForm()
		context = {'query_prob': request.session['Question_problem'], 'query_equa': request.session['Question_problem.5'][1], 'form': form, 'results': request.session['Routines']}
        	return render_to_response('problem.html', context_instance=RequestContext(request, context))







#"Matrixtype" question answered. ---> Question 4: How is your matrix stored?

def search_matrixtype(request):
        form_Type = MatrixTypeForm(request, request.GET or None)
	if form_Type.is_valid():
 		for val in form_Type.fields['question_type'].choices:
			if val[0] == form_Type.cleaned_data['question_type']:
				request.session['Routines'] = request.session['Routines'].filter(matrixType = val[0])
				request.session['Question3'] = [val[0], val[1]]
				form = StorageForm(request)
				context = {'query_prob': request.session['Question_problem'],  'query_comp': request.session.get('Question2')[1], 'query_type': val[1], 'form': form, 'results': request.session['Routines']}
				return render_to_response('matrixtype.html', context_instance=RequestContext(request, context)) 

	
	else:
		form = MatrixTypeForm(request)
		context = {'query_prob': request.session['Question_problem'], 'query_equa': request.session['Question_problem.5'][1], 'query_comp': request.session.get('Question2')[1], 'form': form, 'results': request.session['Routines']}
		return render_to_response('complex.html', context_instance=RequestContext(request, context))





#"Storage" question answered. ---> Question 5: Would you like to use single or double precision?

def search_storage(request):
        form_Stor = StorageForm(request, request.GET or None)
	if form_Stor.is_valid():
 		for val in form_Stor.fields['question_stor'].choices:
			if val[0] == form_Stor.cleaned_data['question_stor']:
				request.session['Routines'] = request.session['Routines'].filter(storageType = val[0])
				request.session['Question4'] = [val[0], val[1]]
				form = PrecisionForm()
				context = {'query_prob': request.session['Question_problem'],  'query_comp': request.session.get('Question2')[1], 'query_type': request.session.get('Question3')[1], 'query_stor': val[1], 'form': form, 'results': request.session['Routines']}
				return render_to_response('storagetype.html', context_instance=RequestContext(request, context)) 

	
	else:
		form = StorageForm(request)
		context = {'query_prob': request.session['Question_problem'], 'query_equa': request.session['Question_problem.5'][1], 'query_comp': request.session.get('Question2')[1], 'query_type': request.session.get('Question3')[1],'form': form_Stor, 'results': request.session['Routines']}
		return render_to_response('matrixtype.html', context_instance=RequestContext(request, context))   






#"Precision" question answered. ---> Final result.

def search_precision(request):
	form_Prec = PrecisionForm(request.GET or None)
	if form_Prec.is_valid():
		if form_Prec.cleaned_data['question_prec'] == unicode('d'):
			val_0 = 'd'	
			val_1 = 'Double'
			if request.session.get('Question2')[0] == 'y':
				request.session['Routines'] = request.session['Routines'].filter(thePrecision = 'z')
			
			if request.session.get('Question2')[0] == 'n':
				request.session['Routines'] = request.session['Routines'].filter(thePrecision = 'd')

		else:
			val_0 = 's'	
			val_1 = 'Single'
			if request.session.get('Question2')[0] == 'y':
				request.session['Routines'] = request.session['Routines'].filter(thePrecision = 'c')
			
			if request.session.get('Question2')[0] == 'n':
				request.session['Routines'] = request.session['Routines'].filter(thePrecision = 's')	

		context = {'query_prob': request.session['Question_problem'], 'query_comp': request.session.get('Question2')[1], 'query_type': request.session.get('Question3')[1], 'query_stor': request.session.get('Question4')[1], 'query_prec': val_1, 'results': request.session['Routines']}
		return render_to_response('precision.html', context_instance=RequestContext(request, context)) 		


	else:
		form = PrecisionForm()
		context = {'query_prob': request.session['Question_problem'], 'query_equa': request.session['Question_problem.5'][1], 'query_comp': request.session.get('Question2')[1], 'query_type': request.session.get('Question3')[1],  'query_stor': request.session.get('Question4')[1], 'form': form, 'results': request.session['Routines']}
		return render_to_response('storagetype.html', context_instance=RequestContext(request, context))










###---------------- General Search ------------------###
def search_result(request):
    error = False
    if 'q' in request.GET:
        Q = request.GET['q']
	q = Q.lower()
        if not q:
            error = True
        else:
            	routines = []
		routines_le_simple = SearchQuerySet().models(LinearEquation_simple).filter(info__istartswith=q)
		routines_le_expert = SearchQuerySet().models(LinearEquation_expert).filter(info__istartswith=q)
            	routines_le_computational = SearchQuerySet().models(LinearEquation_computational).filter(info__istartswith=q)
		routines = list(chain(routines_le_simple, routines_le_expert, routines_le_computational))

		return render_to_response('search_results.html', {'results': routines, 'query': Q})

   
    return render_to_response('search_form.html', {'error': error})






###---------------- Advanced Search ------------------###
def search_advanced(request):
    request.session.clear()	
    return render_to_response('advanced_form.html')



def advanced_result(request):
	form = AdvancedForm(request.GET or None)
	Driver_simple = []
	Computational = []
	Driver_expert = []

	filter_simple = {}
	filter_expert = {}

	if form.is_valid():
		selected = form.cleaned_data['advanced']

	for item in selected:
		if item.startswith("simple"):
			Driver_simple.append(item[7:])
		
		elif item.startswith("computational"):
			Computational.append(item[14:])

		else:
			Driver_expert.append(item[7:])
	

		for item in Driver_simple:
			if item.endswith('yes'):
				if 'precision single' in Driver_simple:
					filter_simple['thePrecision'] = 'c'
				if 'precision double' in Driver_simple:
					filter_simple['thePrecision'] = 'z'				
			if item.endswith('no'):
				if 'precision single' in Driver_simple:
					filter_simple['thePrecision'] = 's'
				if 'precision double' in Driver_simple:
					filter_simple['thePrecision'] = 'd'
				filter_simple['thePrecision__in'] = ['s', 'd']
			if item.startswith('matrix'):
				filter_simple['matrixType'] = item[7:]
			if item.startswith('storage'):
				filter_simple['storageType'] = item[10:]

		
		routines_simple = LinearEquation_simple.objects.filter(**filter_simple)
				


		for item in Driver_expert:
			if item.endswith('yes'):
				if 'precision single' in Driver_expert:
					filter_expert['thePrecision'] = 'c'
				if 'precision double' in Driver_expert:
					filter_expert['thePrecision'] = 'z'				
			if item.endswith('no'):
				if 'precision single' in Driver_expert:
					filter_expert['thePrecision'] = 's'
				if 'precision double' in Driver_expert:
					filter_expert['thePrecision'] = 'd'
				filter_expert['thePrecision__in'] = ['s', 'd']
			if item.startswith('matrix'):
				filter_expert['matrixType'] = item[7:]
			if item.startswith('storage'):
				filter_expert['storageType'] = item[10:] 
			if 'notes equilibrate' in Driver_expert:
				filter_expert['notes__icontains'] = 'equilibrate'		
				
		routines_expert = LinearEquation_expert.objects.filter(**filter_expert)


	context = {'results_simple': routines_simple, 'results_expert': routines_expert}
	return render_to_response('AdvancedSearchResult.html', context_instance=RequestContext(request, context))

