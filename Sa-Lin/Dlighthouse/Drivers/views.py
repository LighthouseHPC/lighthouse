import string
from django.http import HttpResponse, HttpResponseRedirect
from django.shortcuts import render_to_response
from django.contrib.contenttypes.models import ContentType
from django.db.models import get_model
from itertools import chain
from haystack.query import SearchQuerySet
from Drivers.models import Problem, RoutineInfo, LinearEquation, LinearLeastSquare, Eigensolver 
from Drivers.forms import ProblemForm, PrecisionForm, ComplexForm, MatrixTypeForm, StorageForm
from Computational.models import LinearEquation_factor, LinearEquation_solve, LinearEquation_condition_number, LinearEquation_error_bound, LinearEquation_invert, LinearEquation_equilibrate
from Combine.models import LinearEquation_comb, LinearEquation_trans


app_model = {
	'Drivers':		['LinearEquation', 'LinearLeastSquare', 'Eigensolver',],
	'Computational':	['LinearEquation_factor', 'LinearEquation_solve', 'LinearEquation_condition_number', 'LinearEquation_error_bound', 					 'LinearEquation_invert', 'LinearEquation_equilibrate'],
	'Combine':		['LinearEquation_comb', 'LinearEquation_trans'],
}	



#find the application for the chosen model 
def find_app(value):
	for item in app_model.items():
		for model in item[1]:
			if model == value:
				return item[0]
		



#Question 1: What is the type of the problem you would like to solve?

def search_form(request):
    form = ProblemForm()
    request.session.clear() 	
    return render_to_response('search_form.html', {'form': form})




#"Problem" question answered. ---> Question 2: Are there complex numbers in your matrix?

#"request.session" is a dictionary that records the answers: {'Application': Drivers/Computational, 'Question1' : [ , ], 'Question2' : [ , ] ...} 
 
#get_model(appName, modelName)

#Model_List=[]  ---> when calling more than one models for SearchQuerySet().
def search_problem(request):
	form_Prob = ProblemForm(request.GET or None)
	if form_Prob.is_valid():
 		for val in form_Prob.fields['question_prob'].choices:
			if val[0] == form_Prob.cleaned_data['question_prob']:
				### --- Do the following if there are more than one models --- ###
				#ct = ContentType.objects.get(model=val[0])   ###ct: <ContentType: linear equation>
				#Model_List.append(ct.model_class())		###ct.model_class(): <class 'Drivers.models.LinearEquation'>
				#routines = SearchQuerySet().models(*Model_List).all()		
				### --- If use Haystack search --- ###
				#routines = SearchQuerySet().models(get_model(find_app(val[0]), val[0])).all()	

				routines = get_model(find_app(val[0]), val[0]).objects.all()
				form_Comp = ComplexForm()
				request.session['Question1'] = [val[0], val[1]]
				request.session['Application'] = find_app(val[0])
				return render_to_response('problem.html', {'query_prob': val[1], 'form': form_Comp, 'results': routines})
			

	else:
		form_Prob = ProblemForm()
		return render_to_response('search_form.html', {'form': form_Prob})





#"Complex" question answered. ---> Question 3: What is the type of your matrix? 
#MODEL.objects.filter(**condition), where condition must be a dictionary.
#request.session = {'Question1': [], 'Question2': [], ... 'filter_dict': {condition}}

def search_complex(request):
	form_Comp = ComplexForm(request.GET or None)
	if form_Comp.is_valid():
		if form_Comp.cleaned_data['question_comp'] == unicode('y'):
			val_0 = 'y'
			val_1 = 'Yes'
			request.session['filter_dict']={'thePrecision__in': ['c', 'z']}

			
		else:
			val_0 = 'n'
			val_1 = 'No'
			request.session['filter_dict']={'thePrecision__in': ['s', 'd']}
			
		routines = get_model(request.session.get('Application'), request.session.get('Question1')[0]).objects.filter(**request.session['filter_dict'])
		form_Type = MatrixTypeForm(request)	
		request.session['Question2'] = [val_0, val_1] 						
		
		return render_to_response('complex.html', {'query_prob': request.session.get('Question1')[1], 'query_comp': val_1, 'form': form_Type, 'results': routines})

 			
	else:
        	form_Comp = ComplexForm()
		routines = get_model(request.session.get('Application'), request.session.get('Question1')[0]).objects.all()
        	return render_to_response('problem.html', {'query_prob': request.session.get('Question1')[1], 'form': form_Comp, 'results': routines})









#"Matrixtype" question answered. ---> Question 4: How is your matrix stored?

def search_matrixtype(request):
        form_Type = MatrixTypeForm(request, request.GET or None)
	if form_Type.is_valid():
 		for val in form_Type.fields['question_type'].choices:
			if val[0] == form_Type.cleaned_data['question_type']:
				request.session['filter_dict']['matrixType'] = val[0]
				routines = get_model(request.session.get('Application'),  request.session.get('Question1')[0]).objects.filter(**request.session['filter_dict'])
				request.session['Question3'] = [val[0], val[1]]
				form_Stor = StorageForm(request)
				return render_to_response('matrixtype.html', {'query_prob': request.session.get('Question1')[1], 'query_comp': request.session.get('Question2')[1], 'query_type': val[1], 'form': form_Stor, 'results': routines}) 

	
	else:
		form_Type = MatrixTypeForm(request)
		routines = get_model(request.session.get('Application'),  request.session.get('Question1')[0]).objects.filter(**request.session['filter_dict'])
		return render_to_response('complex.html', {'query_prob': request.session.get('Question1')[1], 'query_comp': request.session.get('Question2')[1], 'form': form_Type, 'results': routines})





#"Storage" question answered. ---> Question 5: Would you like to use single or double precision?

def search_storage(request):
        form_Stor = StorageForm(request, request.GET or None)
	if form_Stor.is_valid():
 		for val in form_Stor.fields['question_stor'].choices:
			if val[0] == form_Stor.cleaned_data['question_stor']:
				request.session['filter_dict']['structureType'] = val[0]
				routines = get_model(request.session.get('Application'),  request.session.get('Question1')[0]).objects.filter(**request.session['filter_dict'])
				request.session['Question4'] = [val[0], val[1]]
				form_Prec = PrecisionForm()
				return render_to_response('storagetype.html', {'query_prob': request.session.get('Question1')[1], 'query_comp': request.session.get('Question2')[1], 'query_type': request.session.get('Question3')[1], 'query_stor': val[1], 'form': form_Prec, 'results': routines}) 

	
	else:
		form_Stor = StorageForm(request)
		routines = get_model(request.session.get('Application'),  request.session.get('Question1')[0]).objects.filter(**request.session['filter_dict'])
		return render_to_response('matrixtype.html', {'query_prob': request.session.get('Question1')[1], 'query_comp': request.session.get('Question2')[1], 'query_type': request.session.get('Question3')[1],'form': form_Stor, 'results': routines})   






#"Precision" question answered. ---> Final result.

def search_precision(request):
	form_Prec = PrecisionForm(request.GET or None)
	if form_Prec.is_valid():
		if form_Prec.cleaned_data['question_prec'] == unicode('d'):
			val_0 = 'd'	
			val_1 = 'Double'
			if request.session.get('Question2')[0] == 'y':
				request.session['filter_dict']['thePrecision__in'] = ['z']
			
			if request.session.get('Question2')[0] == 'n':
				request.session['filter_dict']['thePrecision__in'] = ['d']

		else:
			val_0 = 's'	
			val_1 = 'Single'
			if request.session.get('Question2')[0] == 'y':
				request.session['filter_dict']['thePrecision__in'] = ['c']
			
			if request.session.get('Question2')[0] == 'n':
				request.session['filter_dict']['thePrecision__in'] = ['s']	

		routines = get_model(request.session.get('Application'),  request.session.get('Question1')[0]).objects.filter(**request.session['filter_dict'])

		return render_to_response('precision.html', {'query_prob': request.session.get('Question1')[1], 'query_comp': request.session.get('Question2')[1], 'query_type': request.session.get('Question3')[1], 'query_stor': request.session.get('Question4')[1], 'query_prec': val_1, 'results': routines}) 		


	else:
		form_Prec = PrecisionForm()
		routines = get_model(request.session.get('Application'),  request.session.get('Question1')[0]).objects.filter(**request.session['filter_dict'])
		return render_to_response('storagetype.html', {'query_prob': request.session.get('Question1')[1], 'query_comp': request.session.get('Question2')[1], 'query_type': request.session.get('Question3')[1],  'query_stor': request.session.get('Question4')[1], 'form': form_Prec, 'results': routines})


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
			routines_le = LinearEquation.objects.filter(thePrecision ='s')
			routines_lls = LinearLeastSquare.objects.filter(thePrecision ='s')
			routines_eigen = Eigensolver.objects.filter(thePrecision ='s')

			
		elif q == 'double':
			routines_le = LinearEquation.objects.filter(thePrecision ='d')
			routines_lls = LinearLeastSquare.objects.filter(thePrecision ='d')
			routines_eigen = Eigensolver.objects.filter(thePrecision ='d')

			
		elif q == 'complex':
			routines_le = LinearEquation.objects.filter(thePrecision ='c')
			routines_lls = LinearLeastSquare.objects.filter(thePrecision ='c')
			routines_eigen = Eigensolver.objects.filter(thePrecision ='c')

			
		elif q == 'complex16' or q == 'complex 16' or q == 'double complex':
			routines_le = LinearEquation.objects.filter(thePrecision ='z')
			routines_lls = LinearLeastSquare.objects.filter(thePrecision ='z')
			routines_eigen = Eigensolver.objects.filter(thePrecision ='z')

			
#search in "matrix type"
		elif q == 'general' or q == 'spd':
            		routines_le = LinearEquation.objects.filter(matrixType__icontains=q)
			routines_lls = LinearLeastSquare.objects.filter(matrixType__icontains=q)
			routines_eigen = Eigensolver.objects.filter(matrixType__icontains=q)


#search in "'storage type"
		elif q == 'full' or q == 'band' or q == 'banded' or q =='pack' or q == 'packed' or q == 'tridiagonal':
            		routines_le = LinearEquation.objects.filter(structureType__icontains=q)
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
            		routines_le = LinearEquation.objects.filter(description__icontains=q)
			routines_lls = LinearLeastSquare.objects.filter(description__icontains=q)
			routines_eigen = Eigensolver.objects.filter(description__icontains=q)

#search in "info"
		else:
            		routines_le = SearchQuerySet().models(LinearEquation).filter(info__istartswith=q).order_by('id')
			routines_le_factor = SearchQuerySet().models(LinearEquation_factor).filter(info__istartswith=q).order_by('id')
			routines_le_solve = SearchQuerySet().models(LinearEquation_solve).filter(info__istartswith=q).order_by('id')
			routines_le_condition_number = SearchQuerySet().models(LinearEquation_condition_number).filter(info__istartswith=q).order_by('id')
			routines_le_error_bound = SearchQuerySet().models(LinearEquation_error_bound).filter(info__istartswith=q).order_by('id')
			routines_le_invert = SearchQuerySet().models(LinearEquation_invert).filter(info__istartswith=q).order_by('id')
			routines_le_equilibrate = SearchQuerySet().models(LinearEquation_equilibrate).filter(info__istartswith=q).order_by('id')
			

#			routines_le = list(chain(routines_prob_le, routines_info_le))
#			routines_lls = list(chain(routines_prob_lls, routines_info_lls))
#			routines_eigen = list(chain(routines_prob_sym, routines_info_sym))
#			routines_nonsym = list(chain(routines_prob_nonsym, routines_info_nonsym))
			


		
		return render_to_response('search_results.html', {'results_le': routines_le, 'results_le_factor': routines_le_factor, 'results_le_solve': routines_le_solve, 'results_le_condition_number': routines_le_condition_number, 'results_le_error_bound': routines_le_error_bound, 'results_le_invert': routines_le_invert, 'routines_le_equilibrate': routines_le_equilibrate, 'query': Q})



   
    return render_to_response('search_form2.html', 
        {'error': error})





