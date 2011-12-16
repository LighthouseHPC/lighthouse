import datetime
from haystack.indexes import *
from haystack import site
from Drivers.models import LinearEquation
from Drivers.models import LinearLeastSquare
from Drivers.models import Eigensolver
#from Drivers.models import SymmetricEigenvalue
#from Drivers.models import nonSymmetricEigenvalue



class LinearEquationIndex(SearchIndex):
    text = CharField(document=True, use_template=True) 
    thePrecision = CharField(model_attr='thePrecision')
    routineName = CharField(model_attr='routineName')
    matrixType = CharField(model_attr='matrixType')
    structureType = CharField(model_attr='structureType')
    url = CharField(model_attr='url')
    problem = CharField(model_attr='problem')
    description = CharField(model_attr='description')
    info = CharField(model_attr='info')

    def index_queryset(self):
        """Used when the entire index for model is updated."""
	return LinearEquation.objects.all()



class LinearLeastSquareIndex(SearchIndex):
    text = CharField(document=True, use_template=True) 
    thePrecision = CharField(model_attr='thePrecision')
    routineName = CharField(model_attr='routineName')
    matrixType = CharField(model_attr='matrixType')
    structureType = CharField(model_attr='structureType')
    url = CharField(model_attr='url')
    problem = CharField(model_attr='problem')
    description = CharField(model_attr='description')
    info = CharField(model_attr='info')


"""
class SymmetricEigenvalueIndex(SearchIndex):
    text = CharField(document=True, use_template=True) 
    thePrecision = CharField(model_attr='thePrecision')
    routineName = CharField(model_attr='routineName')
    matrixType = CharField(model_attr='matrixType')
    structureType = CharField(model_attr='structureType')
    url = CharField(model_attr='url')
    problem = CharField(model_attr='problem')
    description = CharField(model_attr='description')
    info = CharField(model_attr='info')



class nonSymmetricEigenvalueIndex(SearchIndex):
    text = CharField(document=True, use_template=True) 
    thePrecision = CharField(model_attr='thePrecision')
    routineName = CharField(model_attr='routineName')
    matrixType = CharField(model_attr='matrixType')
    structureType = CharField(model_attr='structureType')
    url = CharField(model_attr='url')
    problem = CharField(model_attr='problem')
    description = CharField(model_attr='description')
    info = CharField(model_attr='info')
"""


class EigensolverIndex(SearchIndex):
    text = CharField(document=True, use_template=True) 
    thePrecision = CharField(model_attr='thePrecision')
    routineName = CharField(model_attr='routineName')
    matrixType = CharField(model_attr='matrixType')
    structureType = CharField(model_attr='structureType')
    url = CharField(model_attr='url')
    problem = CharField(model_attr='problem')
    description = CharField(model_attr='description')
    info = CharField(model_attr='info')


site.register(LinearEquation, LinearEquationIndex)
site.register(LinearLeastSquare, LinearLeastSquareIndex)
#site.register(SymmetricEigenvalue, SymmetricEigenvalueIndex)
#site.register(nonSymmetricEigenvalue, nonSymmetricEigenvalueIndex)
site.register(Eigensolver, EigensolverIndex)




