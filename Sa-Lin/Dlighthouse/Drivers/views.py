import string
from django.http import HttpResponse, HttpResponseRedirect
from django.shortcuts import render_to_response
from django.contrib.contenttypes.models import ContentType
from django.db.models import get_model
from itertools import chain
from haystack.query import SearchQuerySet
from Dlighthouse.Drivers.models import Problem, RoutineInfo, LinearEquation, LinearLeastSquare, Eigensolver 
from Dlighthouse.Drivers.forms import ProblemForm, PrecisionForm, ComplexForm, MatrixTypeForm, StorageForm




def search_form(request):
    form = ProblemForm()
    request.session.clear() 	
    return render_to_response('search_form.html', {'form': form})




#Model_List=[]


def search_problem(request):
	form_Prob = ProblemForm(request.GET or None)
	if form_Prob.is_valid():
 		for val in form_Prob.fields['question_prob'].choices:
			if val[0] == form_Prob.cleaned_data['question_prob']:
				### --- Do the following if there are more than one models --- ###
				#ct = ContentType.objects.get(model=val[0])   ###ct: <ContentType: linear equation>
				#Model_List.append(ct.model_class())		###ct.model_class(): <class 'Drivers.models.LinearEquation'>
				#routines = SearchQuerySet().models(*Model_List).all()			
				routines = SearchQuerySet().models(get_model('Drivers', val[0])).all()	
				form_Prec = PrecisionForm()
				request.session['Question1'] = [val[0], val[1]]
				return render_to_response('problem.html', {'query_prob': val[1], 'form': form_Prec, 'results': routines})
			

	else:
		form_Prob = ProblemForm()
		return render_to_response('search_form.html', {'form': form_Prob})







def search_precision(request):
	form_Prec = PrecisionForm(request.GET or None)
	if form_Prec.is_valid():
		form_Comp = ComplexForm()	
		if form_Prec.cleaned_data['question_prec'] == unicode('d'):
			val_0 = 'd'
			val_1 = 'Double'
			request.session['filter_dict']={'thePrecision__in':['d', 'z']}

			
		else:
			val_0 = 's'
			val_1 = 'Single'
			request.session['filter_dict']={'thePrecision__in':['s', 'c']}
			
		routines = get_model('Drivers', request.session.get('Question1')[0]).objects.filter(**request.session['filter_dict'])
			
		request.session['Question2'] = [val_0, val_1] 						
		
		return render_to_response('precision.html', {'query_prob': request.session.get('Question1')[1], 'query_prec': val_1, 'form': form_Comp, 'results': routines})

 			
	else:
        	form_Prec = PrecisionForm()
		routines = get_model('Drivers', request.session.get('Question1')[0]).objects.all()
        	return render_to_response('problem.html', {'query_prob': request.session.get('Question1')[1], 'form': form_Prec, 'results': routines})







def search_complex(request):
        form_Comp = ComplexForm(request.GET or None)
	if form_Comp.is_valid():
		if form_Comp.cleaned_data['question_comp'] == unicode('y'):
			val_0 = 'y'
			val_1 = 'Yes'
			if request.session.get('Question2')[0] == 's':
				request.session['filter_dict']['thePrecision__in'] = ['c']
			if request.session.get('Question2')[0] == 'd':
				request.session['filter_dict']['thePrecision__in'] = ['z']

		else:
			val_0 = 'n'
			val_1 = 'No'
			if request.session.get('Question2')[0] == 's':
				request.session['filter_dict']['thePrecision__in'] = ['s']
			if request.session.get('Question2')[0] == 'd':
				request.session['filter_dict']['thePrecision__in'] = ['d']
			
		routines = get_model('Drivers', request.session.get('Question1')[0]).objects.filter(**request.session['filter_dict'])

		request.session['Question3'] = [val_0, val_1] 
		form_Type = MatrixTypeForm(request)	
		
        	return render_to_response('complex.html', {'query_prob': request.session.get('Question1')[1], 'query_prec': request.session.get('Question2')[1], 'query_comp': val_1, 'form': form_Type, 'results': routines})

	else:
		form_Comp = ComplexForm()
		routines = get_model('Drivers', request.session.get('Question1')[0]).objects.filter(**request.session['filter_dict'])
		return render_to_response('precision.html', {'query_prob': request.session.get('Question1')[1], 'query_prec': request.session.get('Question2')[1], 'form': form_Comp, 'results': routines})







def search_matrixtype(request):
        form_Type = MatrixTypeForm(request, request.GET or None)
	if form_Type.is_valid():
 		for val in form_Type.fields['question_type'].choices:
			if val[0] == form_Type.cleaned_data['question_type']:
				request.session['filter_dict']['matrixType'] = val[0]
				routines = get_model('Drivers', request.session.get('Question1')[0]).objects.filter(**request.session['filter_dict'])
				request.session['Question4'] = [val[0], val[1]]
				form_Stor = StorageForm(request)
				return render_to_response('matrixtype.html', {'query_prob': request.session.get('Question1')[1], 'query_prec': request.session.get('Question2')[1], 'query_comp': request.session.get('Question3')[1], 'query_type': val[1], 'form': form_Stor, 'results': routines}) 

	
	else:
		form_Type = MatrixTypeForm(request)
		routines = get_model('Drivers', request.session.get('Question1')[0]).objects.filter(**request.session['filter_dict'])
		return render_to_response('complex.html', {'query_prob': request.session.get('Question1')[1], 'query_prec': request.session.get('Question2')[1], 'query_comp': request.session.get('Question3')[1], 'form': form_Type, 'results': routines})






def search_storage(request):
        form_Stor = StorageForm(request, request.GET or None)
	if form_Stor.is_valid():
 		for val in form_Stor.fields['question_stor'].choices:
			if val[0] == form_Stor.cleaned_data['question_stor']:
				request.session['filter_dict']['structureType'] = val[0]
				routines = get_model('Drivers', request.session.get('Question1')[0]).objects.filter(**request.session['filter_dict'])
				request.session['Question5'] = [val[0], val[1]]
				return render_to_response('storagetype.html', {'query_prob': request.session.get('Question1')[1], 'query_prec': request.session.get('Question2')[1], 'query_comp': request.session.get('Question3')[1], 'query_type': request.session.get('Question4')[1], 'query_stor': val[1], 'results': routines}) 

	
	else:
		form_Stor = StorageForm(request)
		routines = get_model('Drivers', request.session.get('Question1')[0]).objects.filter(**request.session['filter_dict'])
		return render_to_response('matrixtype.html', {'query_prob': request.session.get('Question1')[1], 'query_prec': request.session.get('Question2')[1], 'query_comp': request.session.get('Question3')[1], 'query_type': request.session.get('Question4')[1],'form': form_Stor, 'results': routines})   







###---------------- General Search ------------------###
"""

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
		routines_sym = []
		routines_nonsym = []
	
#search in "precision"
		if q == 'single':
			routines_le = LinearEquation.objects.filter(thePrecision ='s')
			routines_lls = LinearLeastSquare.objects.filter(thePrecision ='s')
			routines_sym = SymmetricEigenvalue.objects.filter(thePrecision ='s')
			routines_nonsym = nonSymmetricEigenvalue.objects.filter(thePrecision ='s')
			
		elif q == 'double':
			routines_le = LinearEquation.objects.filter(thePrecision ='d')
			routines_lls = LinearLeastSquare.objects.filter(thePrecision ='d')
			routines_sym = SymmetricEigenvalue.objects.filter(thePrecision ='d')
			routines_nonsym = nonSymmetricEigenvalue.objects.filter(thePrecision ='d')
			
		elif q == 'complex':
			routines_le = LinearEquation.objects.filter(thePrecision ='c')
			routines_lls = LinearLeastSquare.objects.filter(thePrecision ='c')
			routines_sym = SymmetricEigenvalue.objects.filter(thePrecision ='c')
			routines_nonsym = nonSymmetricEigenvalue.objects.filter(thePrecision ='c')
			
		elif q == 'complex16' or q == 'complex 16' or q == 'double complex':
			routines_le = LinearEquation.objects.filter(thePrecision ='z')
			routines_lls = LinearLeastSquare.objects.filter(thePrecision ='z')
			routines_sym = SymmetricEigenvalue.objects.filter(thePrecision ='z')
			routines_nonsym = nonSymmetricEigenvalue.objects.filter(thePrecision ='z')
			
#search in "matrix type"
		elif q == 'general' or q == 'spd':
            		routines_le = LinearEquation.objects.filter(matrixType__icontains=q)
			routines_lls = LinearLeastSquare.objects.filter(matrixType__icontains=q)
			routines_sym = SymmetricEigenvalue.objects.filter(matrixType__icontains=q)
			routines_nonsym = nonSymmetricEigenvalue.objects.filter(matrixType__icontains=q)


#search in "'storage type"
		elif q == 'full' or q == 'band' or q == 'banded' or q =='pack' or q == 'packed' or q == 'tridiagonal':
            		routines_le = LinearEquation.objects.filter(structureType__icontains=q)
			routines_lls = LinearLeastSquare.objects.filter(structureType__icontains=q)
			routines_sym = SymmetricEigenvalue.objects.filter(structureType__icontains=q)
			routines_nonsym = nonSymmetricEigenvalue.objects.filter(structureType__icontains=q)



#search according to the models
		elif q == 'linear equation' or q == 'a * x = b' or q == 'ax=b':
            		routines_le = SearchQuerySet().models(LinearEquation).all()

		elif q == 'linear least square':
			routines_lls = SearchQuerySet().models(LinearLeastSquare).all()

		elif q == 'symmetric eigenvalue' or q == 'symmetric eigenvalues':
			routines_sym = SearchQuerySet().models(SymmetricEigenvalue).all()

		elif q == 'nonsymmetric eigenvalue' or q == 'nonsymmetric eigenvalues':
			routines_nonsym = SearchQuerySet().models(nonSymmetricEigenvalue).all()


#search in "description"
		elif q == 'eigen' or q == 'eigenvalue'or q == 'eigenvalues' or q == 'eigenvector' or q == 'eigenvectors':
            		routines_le = LinearEquation.objects.filter(description__icontains=q)
			routines_lls = LinearLeastSquare.objects.filter(description__icontains=q)
			routines_sym = SymmetricEigenvalue.objects.filter(description__icontains=q)
			routines_nonsym = nonSymmetricEigenvalue.objects.filter(description__icontains=q)

#search in "info"
		else:
            		routines_le = SearchQuerySet().models(LinearEquation).filter(info__istartswith=q)
			routines_lls = SearchQuerySet().models(LinearLeastSquare).filter(info__istartswith=q)
			routines_sym = SearchQuerySet().models(SymmetricEigenvalue).filter(info__istartswith=q)
			routines_nonsym = SearchQuerySet().models(nonSymmetricEigenvalue).filter(info__istartswith=q)


#			routines_le = list(chain(routines_prob_le, routines_info_le))
#			routines_lls = list(chain(routines_prob_lls, routines_info_lls))
#			routines_sym = list(chain(routines_prob_sym, routines_info_sym))
#			routines_nonsym = list(chain(routines_prob_nonsym, routines_info_nonsym))
			


		
		return render_to_response('search_results.html', {'results_le': routines_le, 'results_lls': routines_lls, 'results_sym': routines_sym, 'results_nonsym': routines_nonsym, 'query': Q})



   
    return render_to_response('search_form.html', 
        {'error': error})

"""



