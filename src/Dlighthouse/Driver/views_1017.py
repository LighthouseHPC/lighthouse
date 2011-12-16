import string
from django.http import HttpResponse, HttpResponseRedirect
from django.shortcuts import render_to_response
from django.contrib.contenttypes.models import ContentType
from django.db.models import get_model, Q
from itertools import chain
from haystack.query import SearchQuerySet
from Drivers.forms import ProblemForm, EquationForm, PrecisionForm, ComplexForm, MatrixTypeForm, StructureForm, SimpleForm
from Drivers.models import RoutineInfo, LinearEquation_simple, LinearEquation_expert 
from Computational.models import LinearEquation_computational
from django.template import RequestContext



app_model = {
	'Drivers':		['LinearEquation_simple', 'LinearEquation_expert',],
	'Computational':	['LinearEquation_computational'],
}	



#find the application for the chosen model 
def find_app(value):
	for item in app_model.items():
		for model in item[1]:
			if model == value:
				return item[0]
		


#link (or) the Q's.
def link_Q(aList):
	query = Q()
	for value in aList:
		query |= value
	return query	



#Question 1: What is the type of the problem you would like to solve?

def search_form(request):
    form = ProblemForm()
    request.session.clear() 	
    return render_to_response('search_form.html', {'form': form})




#"Problem" question answered. ---> Question 1.5: What form of the linear system do you want to solve? OR Question 2: Are there complex numbers in your matrix? 

#MODEL.objects.filter(**condition), where condition must be a dictionary.
#"request.session" is a dictionary that records the answers: request.session = {'Routines': [], 'Question1': [], 'Question2': [], ...}
#get_model(appName, modelName)
#form.cleaned_data and request.GET.getlist do the same thing --> list the chosen options.
#Model_List=[]  ---> when calling more than one models for SearchQuerySet().
def search_problem(request):
	form_Prob = ProblemForm(request.GET or None)
	Answers = []
	if form_Prob.is_valid():
		selected = form_Prob.cleaned_data['question_prob']	
		queries = [Q(notes__icontains=item) for item in selected]
		for answer in selected:
			Answers.append((answer, ProblemForm().find(answer))) 

				

		if selected == [u'LinearEquation']:
			routines = LinearEquation_simple.objects.all()


		elif 'LinearEquation' not in selected:	
			routines = LinearEquation_computational.objects.filter(link_Q(queries))
			if 'solve' in selected:
				queries.pop()
	

		else:
			if 'solve' not in selected and 'inverse' not in selected:
				queries.pop(0)
				routines = LinearEquation_expert.objects.filter(link_Q(queries))
				if 'LinearEquation' in selected:
					routines_trans = LinearEquation_expert.objects.filter(link_Q(queries)|Q(notes__icontains='TRANS'))

			elif selected == [u'LinearEquation', u'solve'] or selected == [u'LinearEquation', u'inverse'] :
				queries.pop(0)
				routines = chain(LinearEquation_simple.objects.all(), LinearEquation_computational.objects.filter(link_Q(queries)))
				
			elif selected == [u'LinearEquation', u'solve', u'inverse'] :
				queries.pop(0)
				routines = chain(LinearEquation_simple.objects.all(), LinearEquation_computational.objects.filter(link_Q(queries)))

			else:
				if 'solve' in selected or 'inverse' in selected:
					queries.pop(0)
					queries_comp = queries.pop()
					routines = chain(LinearEquation_expert.objects.filter(link_Q(queries)), 	
							LinearEquation_computational.objects.filter(queries_comp))
				elif 'solve' in selected and 'inverse' in selected:
					queries.pop(0)
					queries.pop()
					queries.pop()
					routines = chain(LinearEquation_expert.objects.filter(link_Q(queries)), 
							LinearEquation_computational(Q(notes__icontains='solve') | Q(notes__icontains='inverse')))

		request.session['Question1'] = Answers
		request.session['Routines'] = routines

		if 'solve' in selected or 'LinearEquation' in selected:
			form = EquationForm()
			action = '/search/problem/equation/'
		else:
			form = ComplexForm()
			action = '/search/problem/complex/'
			request.session['Question1.5']=[0, 0]

		context = {'query_prob': Answers, 'form': form, 'results': routines, 'Action': action}
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
			if 'LinearEquation' in request.session['Question1'][0]:
				request.session['Routines'] = LinearEquation_expert.objects.filter(notes__icontains='TRANS').filter(Q(thePrecision='s')|Q(thePrecision='d'))
			if 'solve' in request.session['Question1'][-1]:
				request.session['Routines'] = LinearEquation_computational.objects.filter(notes__icontains='TRANS').filter(Q(thePrecision='s')|Q(thePrecision='d'))


		elif form_Equa.cleaned_data['question_equa'] == unicode('Hermitian_trans'):
			val_0 = 'Hermitian_trans'
			val_1 = 'A**H*X = B'
			if 'LinearEquation' in request.session['Question1'][0]:
				request.session['Routines'] = LinearEquation_expert.objects.filter(notes__icontains='TRANS').filter(Q(thePrecision='c')|Q(thePrecision='z'))
			if 'solve' in request.session['Question1'][-1]:
				request.session['Routines'] = LinearEquation_computational.objects.filter(notes__icontains='TRANS').filter(Q(thePrecision='c')|Q(thePrecision='z'))

		else:
			val_0 = 'original'
			val_1 = 'A*X = B'
			if 'LinearEquation' in request.session['Question1'][0]:
				request.session['Routines'] = LinearEquation_simple.objects.all()
			if 'solve' in request.session['Question1'][-1]:
				request.session['Routines'] = LinearEquation_computational.objects.filter(notes__icontains='solve')



		request.session['Question1.5'] = [val_0, val_1] 
		form = ComplexForm()
		context = {'query_prob': request.session['Question1'], 'query_equa': val_1, 'form': form, 'results': request.session['Routines']}					
		
		return render_to_response('equation.html', context_instance=RequestContext(request, context))

 			
	else:
        	form = EquationForm()
		context = {'query_prob': request.session['Question1'], 'form': form, 'results': request.session['Routines']}
        	return render_to_response('problem.html', context_instance=RequestContext(request, context))






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
		context = {'query_prob': request.session['Question1'], 'query_equa': request.session['Question1.5'][1], 'query_comp': val_1, 'form': form, 'results': request.session['Routines']}					
		
		return render_to_response('complex.html', context_instance=RequestContext(request, context))

 			
	else:
        	form = ComplexForm()
		context = {'query_prob': request.session['Question1'], 'form': form, 'results': request.session['Routines']}
        	return render_to_response('problem.html', context_instance=RequestContext(request, context))







#"Matrixtype" question answered. ---> Question 4: How is your matrix stored?

def search_matrixtype(request):
        form_Type = MatrixTypeForm(request, request.GET or None)
	if form_Type.is_valid():
 		for val in form_Type.fields['question_type'].choices:
			if val[0] == form_Type.cleaned_data['question_type']:
				request.session['Routines'] = request.session['Routines'].filter(matrixType = val[0])
				request.session['Question3'] = [val[0], val[1]]
				form = StructureForm(request)
				context = {'query_prob': request.session['Question1'], 'query_equa': request.session['Question1.5'][1], 'query_comp': request.session.get('Question2')[1], 'query_type': val[1], 'form': form, 'results': request.session['Routines']}
				return render_to_response('matrixtype.html', context_instance=RequestContext(request, context)) 

	
	else:
		form = MatrixTypeForm(request)
		context = {'query_prob': request.session['Question1'], 'query_equa': request.session['Question1.5'][1], 'query_comp': request.session.get('Question2')[1], 'form': form, 'results': request.session['Routines']}
		return render_to_response('complex.html', context_instance=RequestContext(request, context))





#"Structure" question answered. ---> Question 5: Would you like to use single or double precision?

def search_structure(request):
        form_Stor = StructureForm(request, request.GET or None)
	if form_Stor.is_valid():
 		for val in form_Stor.fields['question_stor'].choices:
			if val[0] == form_Stor.cleaned_data['question_stor']:
				request.session['Routines'] = request.session['Routines'].filter(structureType = val[0])
				request.session['Question4'] = [val[0], val[1]]
				form = PrecisionForm()
				context = {'query_prob': request.session['Question1'], 'query_equa': request.session['Question1.5'][1], 'query_comp': request.session.get('Question2')[1], 'query_type': request.session.get('Question3')[1], 'query_stor': val[1], 'form': form, 'results': request.session['Routines']}
				return render_to_response('structuretype.html', context_instance=RequestContext(request, context)) 

	
	else:
		form = StructureForm(request)
		context = {'query_prob': request.session['Question1'], 'query_equa': request.session['Question1.5'][1], 'query_comp': request.session.get('Question2')[1], 'query_type': request.session.get('Question3')[1],'form': form_Stor, 'results': request.session['Routines']}
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

		context = {'query_prob': request.session['Question1'], 'query_comp': request.session.get('Question2')[1], 'query_type': request.session.get('Question3')[1], 'query_stor': request.session.get('Question4')[1], 'query_prec': val_1, 'results': request.session['Routines']}
		return render_to_response('precision.html', context_instance=RequestContext(request, context)) 		


	else:
		form = PrecisionForm()
		context = {'query_prob': request.session['Question1'], 'query_comp': request.session.get('Question2')[1], 'query_type': request.session.get('Question3')[1],  'query_stor': request.session.get('Question4')[1], 'form': form, 'results': request.session['Routines']}
		return render_to_response('structuretype.html', context_instance=RequestContext(request, context))


###---------------- General Search ------------------###

def search_form2(request):
    return render_to_response('search_form2.html')





def search_result(request):
    error = False
    if 'q' in request.GET:
        Q = request.GET['q']
	q = Q.lower()
        if not q:
            error = True
        else:
            	routines_le = []
		routines_lls = []
		routines_eigen = []
		
#search in "precision"
		if q == 'single':
			routines_le = LinearEquation_all.objects.filter(thePrecision ='s')
			routines_lls = LinearLeastSquare.objects.filter(thePrecision ='s')
			routines_eigen = Eigensolver.objects.filter(thePrecision ='s')

			
		elif q == 'double':
			routines_le = LinearEquation_all.objects.filter(thePrecision ='d')
			routines_lls = LinearLeastSquare.objects.filter(thePrecision ='d')
			routines_eigen = Eigensolver.objects.filter(thePrecision ='d')

			
		elif q == 'complex':
			routines_le = LinearEquation_all.objects.filter(thePrecision ='c')
			routines_lls = LinearLeastSquare.objects.filter(thePrecision ='c')
			routines_eigen = Eigensolver.objects.filter(thePrecision ='c')

			
		elif q == 'complex16' or q == 'complex 16' or q == 'double complex':
			routines_le = LinearEquation_all.objects.filter(thePrecision ='z')
			routines_lls = LinearLeastSquare.objects.filter(thePrecision ='z')
			routines_eigen = Eigensolver.objects.filter(thePrecision ='z')

			
#search in "matrix type"
		elif q == 'general' or q == 'spd':
            		routines_le = LinearEquation_all.objects.filter(matrixType__icontains=q)
			routines_lls = LinearLeastSquare.objects.filter(matrixType__icontains=q)
			routines_eigen = Eigensolver.objects.filter(matrixType__icontains=q)


#search in "'structure type"
		elif q == 'full' or q == 'band' or q == 'banded' or q =='pack' or q == 'packed' or q == 'tridiagonal':
            		routines_le = LinearEquation_all.objects.filter(structureType__icontains=q)
			routines_lls = LinearLeastSquare.objects.filter(structureType__icontains=q)
			routines_eigen = Eigensolver.objects.filter(structureType__icontains=q)



#search according to the models
		elif q == 'linear equation' or q == 'a * x = b' or q == 'ax=b':
            		routines_le = SearchQuerySet().models(LinearEquation).all()

		elif q == 'linear least square':
			routines_lls = SearchQuerySet().models(LinearLeastSquare).all()

		elif q == 'symmetric eigenvalue' or q == 'symmetric eigenvalues':
			routines_eigen = SearchQuerySet().models(Eigensolver).all()



#search in "description"
		elif q == 'eigen' or q == 'eigenvalue'or q == 'eigenvalues' or q == 'eigenvector' or q == 'eigenvectors':
            		routines_le = LinearEquation_all.objects.filter(description__icontains=q)
			routines_lls = LinearLeastSquare.objects.filter(description__icontains=q)
			routines_eigen = Eigensolver.objects.filter(description__icontains=q)

#search in "info"
		else:
            		routines_le = SearchQuerySet().models(LinearEquation).filter(info__istartswith=q).order_by('id')
			#routines_le_factor = SearchQuerySet().models(LinearEquation_factor).filter(info__istartswith=q).order_by('id')
			#routines_le_solve = SearchQuerySet().models(LinearEquation_solve).filter(info__istartswith=q).order_by('id')
			#routines_le_condition_number = SearchQuerySet().models(LinearEquation_condition_number).filter(info__istartswith=q).order_by('id')
			#routines_le_error_bound = SearchQuerySet().models(LinearEquation_error_bound).filter(info__istartswith=q).order_by('id')
			#routines_le_invert = SearchQuerySet().models(LinearEquation_invert).filter(info__istartswith=q).order_by('id')
			#routines_le_equilibrate = SearchQuerySet().models(LinearEquation_equilibrate).filter(info__istartswith=q).order_by('id')
			

#			routines_le = list(chain(routines_prob_le, routines_info_le))
#			routines_lls = list(chain(routines_prob_lls, routines_info_lls))
#			routines_eigen = list(chain(routines_prob_sym, routines_info_sym))
#			routines_nonsym = list(chain(routines_prob_nonsym, routines_info_nonsym))
			


		return render_to_response('search_results.html', {'results_le': routines_le, 'query': Q})

#		return render_to_response('search_results.html', {'results_le': routines_le, 'results_le_factor': routines_le_factor, 'results_le_solve': routines_le_solve, 'results_le_condition_number': routines_le_condition_number, 'results_le_error_bound': routines_le_error_bound, 'results_le_invert': routines_le_invert, 'routines_le_equilibrate': routines_le_equilibrate, 'query': Q})


   
    return render_to_response('search_form2.html', 
        {'error': error})




def datagrid(request):        
    context = {'appname': 'Drivers', 'modelname': 'LinearEquation'}
    return render_to_response("datagrid.html", context)


def grid(request):
    routines = SearchQuerySet().models(LinearEquation).filter(thePrecision__istartswith='s').order_by('id')
    context = {'routine': routines}
    return render_to_response("grid.html", context)




def checkbox(request):
	form = SimpleForm()
	return render_to_response("checkbox.html", {'form': form})





CHECKBOX_CHOICES = (('A','The first choice'),('B','The second Choice'),('C','The third choice'),('D','The forth Choice'))



def handle(request):
	findList = []
	if request.method=='GET':
  		form = SimpleForm(request.GET)
		answerlist = request.GET
		q = request.GET.getlist('pick')
		val = form.fields['pick'].choices
		
		if form.is_valid():
    			cleanData = form.cleaned_data['pick']
			if 'A' not in form.cleaned_data['pick']:
				for send in cleanData:
					findList.append(SimpleForm().find(send))
				
	return render_to_response("handle.html", context_instance=RequestContext(request, {'form': form, 'Answers': answerlist, 'q': q, 'val': val, 'cleanData': cleanData, 'List': findList}))


