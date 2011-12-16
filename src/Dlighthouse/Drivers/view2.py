from django.http import HttpResponse
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
        q = request.GET['q']
        if not q:
            error = True
        else:
		if q == 'single':
			routines_le = LinearEquation.objects.filter(thePrecision ='s')
			routines_lls = LinearEquation.objects.filter(thePrecision ='s')
			routines_sym = LinearEquation.objects.filter(thePrecision ='s')
			routines_nonsym = LinearEquation.objects.filter(thePrecision ='s')
			routines_list = list(chain(routines_le, routines_lls, routines_sym, routines_nonsym))
		elif q == 'double':
			routines_le = LinearEquation.objects.filter(thePrecision ='d')
			routines_lls = LinearEquation.objects.filter(thePrecision ='d')
			routines_sym = LinearEquation.objects.filter(thePrecision ='d')
			routines_nonsym = LinearEquation.objects.filter(thePrecision ='d')
			routines_list = list(chain(routines_le, routines_lls, routines_sym, routines_nonsym))
		elif q == 'complex':
			routines_le = LinearEquation.objects.filter(thePrecision ='c')
			routines_lls = LinearEquation.objects.filter(thePrecision ='c')
			routines_sym = LinearEquation.objects.filter(thePrecision ='c')
			routines_nonsym = LinearEquation.objects.filter(thePrecision ='c')
			routines_list = list(chain(routines_le, routines_lls, routines_sym, routines_nonsym))
		elif q == 'complex16' or q == 'complex 16' or q == 'double complex':
			routines_le = LinearEquation.objects.filter(thePrecision ='z')
			routines_lls = LinearEquation.objects.filter(thePrecision ='z')
			routines_sym = LinearEquation.objects.filter(thePrecision ='z')
			routines_nonsym = LinearEquation.objects.filter(thePrecision ='z')
			routines_list = list(chain(routines_le, routines_lls, routines_sym, routines_nonsym))

		else:
            		routines_le_1 = LinearEquation.objects.filter(matrixType__icontains=q)
			routines_lls_1 = LinearLeastSquare.objects.filter(matrixType__icontains=q)
			routines_sym_1 = SymmetricEigenvalue.objects.filter(matrixType__icontains=q)
			routines_nonsym_1 = nonSymmetricEigenvalue.objects.filter(matrixType__icontains=q)

            		routines_le_2 = LinearEquation.objects.filter(structureType__icontains=q)
			routines_lls_2 = LinearLeastSquare.objects.filter(structureType__icontains=q)
			routines_sym_2 = SymmetricEigenvalue.objects.filter(structureType__icontains=q)
			routines_nonsym_2 = nonSymmetricEigenvalue.objects.filter(structureType__icontains=q)

            		routines_le_3 = LinearEquation.objects.filter(description__icontains=q)
			routines_lls_3 = LinearLeastSquare.objects.filter(description__icontains=q)
			routines_sym_3 = SymmetricEigenvalue.objects.filter(description__icontains=q)
			routines_nonsym_3 = nonSymmetricEigenvalue.objects.filter(description__icontains=q)

            		routines_info = SearchQuerySet().models(LinearEquation, LinearLeastSquare, SymmetricEigenvalue, nonSymmetricEigenvalue).filter(info__startswith=q)



			routines_list = list(chain(routines_le_1, routines_lls_1, routines_sym_1, routines_nonsym_1, routines_le_2, routines_lls_2, routines_sym_2, routines_nonsym_2, routines_le_3, routines_lls_3, routines_sym_3, routines_nonsym_3, routines_info))

		
		return render_to_response('search_results2.html', {'results': routines_list, 'query': q})


	


    return render_to_response('search_form.html',
        {'error': error})
