import string
from django.http import HttpResponse, HttpResponseRedirect
from django.shortcuts import render_to_response
from Dlighthouse.Drivers.models import Problem, RoutineInfo, LinearEquation, LinearLeastSquare, SymmetricEigenvalue, nonSymmetricEigenvalue 
from itertools import chain
from haystack.query import SearchQuerySet

Table_Names = (LinearEquation, LinearLeastSquare, SymmetricEigenvalue, nonSymmetricEigenvalue)


def search_form(request):
    return render_to_response('search_form.html')


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




querry = []



def search_problem(request):
	selProb = request.GET['selProb']
	querry.append(selProb)
	if selProb == 'LinearEquation':
		routines = SearchQuerySet().models(LinearEquation).all()
		return render_to_response('problem.html', {'results': routines, 'query_prob': selProb})



def search_precision(request):
	selPrec = request.GET['selPrec']
	querry.append(selPrec)
	if selPrec == 'single':
		routines = list(chain(LinearEquation.objects.filter(thePrecision ='s'), LinearEquation.objects.filter(thePrecision ='c')))

	else:
		routines = list(chain(LinearEquation.objects.filter(thePrecision ='d'), LinearEquation.objects.filter(thePrecision ='z')))
		
	return render_to_response('precision.html', {'results': routines, 'query_prec': selPrec, 'query_prob': querry[0]})



#def search_complex(request):
#	selComp = request.GET['selComp']
#	querry.append(selComp)
#	if selComp == 'yes':
#		routines = LinearEquation.objects.filter(Precision_refine=search_refine[])

