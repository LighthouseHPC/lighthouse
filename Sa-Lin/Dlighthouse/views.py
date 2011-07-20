from django.http import HttpResponse
from django.shortcuts import render_to_response
from Dlighthouse.Drivers.models import Problem, RoutineInfo, LinearEquation, LinearLeastSquare, SymmetricEigenvalue, nonSymmetricEigenvalue 
from itertools import chain
from haystack.views import SearchView


Table_Names = (LinearEquation, LinearLeastSquare, SymmetricEigenvalue, nonSymmetricEigenvalue)



