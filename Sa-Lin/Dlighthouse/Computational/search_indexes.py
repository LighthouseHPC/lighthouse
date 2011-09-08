import datetime
from haystack.indexes import *
from haystack import site
from Computational.models import LinearEquation_factor, LinearEquation_solve, LinearEquation_condition_number, LinearEquation_error_bound, LinearEquation_invert, LinearEquation_equilibrate
#from Computational.models import Eigensolver_Comput



class LinearEquation_factorIndex(SearchIndex):
    text = CharField(document=True, use_template=True) 
    thePrecision = CharField(model_attr='thePrecision')
    routineName = CharField(model_attr='routineName')
    matrixType = CharField(model_attr='matrixType')
    structureType = CharField(model_attr='structureType')
    url = CharField(model_attr='url')
    problem = CharField(model_attr='problem')
    description = CharField(model_attr='description')
    info = CharField(model_attr='info')


class LinearEquation_solveIndex(SearchIndex):
    text = CharField(document=True, use_template=True) 
    thePrecision = CharField(model_attr='thePrecision')
    routineName = CharField(model_attr='routineName')
    matrixType = CharField(model_attr='matrixType')
    structureType = CharField(model_attr='structureType')
    url = CharField(model_attr='url')
    problem = CharField(model_attr='problem')
    description = CharField(model_attr='description')
    info = CharField(model_attr='info')


class LinearEquation_condition_numberIndex(SearchIndex):
    text = CharField(document=True, use_template=True) 
    thePrecision = CharField(model_attr='thePrecision')
    routineName = CharField(model_attr='routineName')
    matrixType = CharField(model_attr='matrixType')
    structureType = CharField(model_attr='structureType')
    url = CharField(model_attr='url')
    problem = CharField(model_attr='problem')
    description = CharField(model_attr='description')
    info = CharField(model_attr='info')


class LinearEquation_error_boundIndex(SearchIndex):
    text = CharField(document=True, use_template=True) 
    thePrecision = CharField(model_attr='thePrecision')
    routineName = CharField(model_attr='routineName')
    matrixType = CharField(model_attr='matrixType')
    structureType = CharField(model_attr='structureType')
    url = CharField(model_attr='url')
    problem = CharField(model_attr='problem')
    description = CharField(model_attr='description')
    info = CharField(model_attr='info')



class LinearEquation_invertIndex(SearchIndex):
    text = CharField(document=True, use_template=True) 
    thePrecision = CharField(model_attr='thePrecision')
    routineName = CharField(model_attr='routineName')
    matrixType = CharField(model_attr='matrixType')
    structureType = CharField(model_attr='structureType')
    url = CharField(model_attr='url')
    problem = CharField(model_attr='problem')
    description = CharField(model_attr='description')
    info = CharField(model_attr='info')


class LinearEquation_equilibrateIndex(SearchIndex):
    text = CharField(document=True, use_template=True) 
    thePrecision = CharField(model_attr='thePrecision')
    routineName = CharField(model_attr='routineName')
    matrixType = CharField(model_attr='matrixType')
    structureType = CharField(model_attr='structureType')
    url = CharField(model_attr='url')
    problem = CharField(model_attr='problem')
    description = CharField(model_attr='description')
    info = CharField(model_attr='info')

'''
class Eigensolver_ComputIndex(SearchIndex):
    text = CharField(document=True, use_template=True) 
    thePrecision = CharField(model_attr='thePrecision')
    routineName = CharField(model_attr='routineName')
    matrixType = CharField(model_attr='matrixType')
    structureType = CharField(model_attr='structureType')
    url = CharField(model_attr='url')
    problem = CharField(model_attr='problem')
    description = CharField(model_attr='description')
    info = CharField(model_attr='info')
'''



site.register(LinearEquation_factor, LinearEquation_factorIndex)
site.register(LinearEquation_solve, LinearEquation_solveIndex)
site.register(LinearEquation_condition_number, LinearEquation_condition_numberIndex)
site.register(LinearEquation_error_bound, LinearEquation_error_boundIndex)
site.register(LinearEquation_invert, LinearEquation_invertIndex)
site.register(LinearEquation_equilibrate, LinearEquation_equilibrateIndex)
#site.register(Eigensolver_Comput, Eigensolver_ComputIndex)




