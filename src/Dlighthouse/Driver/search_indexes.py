import datetime
from haystack.indexes import *
from haystack import site
from Driver.models import LinearEquation_simple, LinearEquation_expert, LinearEquation_driver
from Driver.models import LinearLeastSquare
from Driver.models import Eigensolver
#from Driver.models import SymmetricEigenvalue
#from Driver.models import nonSymmetricEigenvalue



class LinearEquation_simpleIndex(SearchIndex):
    text = CharField(document=True, use_template=True) 
    thePrecision = CharField(model_attr='thePrecision')
    routineName = CharField(model_attr='routineName')
    matrixType = CharField(model_attr='matrixType')
    storageType = CharField(model_attr='storageType')
    url = CharField(model_attr='url')
    notes = CharField(model_attr='notes')
    info = CharField(model_attr='info')




class LinearEquation_expertIndex(SearchIndex):
    text = CharField(document=True, use_template=True) 
    thePrecision = CharField(model_attr='thePrecision')
    routineName = CharField(model_attr='routineName')
    matrixType = CharField(model_attr='matrixType')
    storageType = CharField(model_attr='storageType')
    url = CharField(model_attr='url')
    notes = CharField(model_attr='notes')
    info = CharField(model_attr='info')





class LinearEquation_driverIndex(SearchIndex):
    text = CharField(document=True, use_template=True) 
    thePrecision = CharField(model_attr='thePrecision')
    routineName = CharField(model_attr='routineName')
    matrixType = CharField(model_attr='matrixType')
    storageType = CharField(model_attr='storageType')
    url = CharField(model_attr='url')
    notes = CharField(model_attr='notes')
    info = CharField(model_attr='info')

 



class LinearLeastSquareIndex(SearchIndex):
    text = CharField(document=True, use_template=True) 
    thePrecision = CharField(model_attr='thePrecision')
    routineName = CharField(model_attr='routineName')
    matrixType = CharField(model_attr='matrixType')
    storageType = CharField(model_attr='storageType')
    url = CharField(model_attr='url')
    notes = CharField(model_attr='notes')
    info = CharField(model_attr='info')


"""
class SymmetricEigenvalueIndex(SearchIndex):
    text = CharField(document=True, use_template=True) 
    thePrecision = CharField(model_attr='thePrecision')
    routineName = CharField(model_attr='routineName')
    matrixType = CharField(model_attr='matrixType')
    storageType = CharField(model_attr='storageType')
    url = CharField(model_attr='url')
    notes = CharField(model_attr='notes')
    info = CharField(model_attr='info')



class nonSymmetricEigenvalueIndex(SearchIndex):
    text = CharField(document=True, use_template=True) 
    thePrecision = CharField(model_attr='thePrecision')
    routineName = CharField(model_attr='routineName')
    matrixType = CharField(model_attr='matrixType')
    storageType = CharField(model_attr='storageType')
    url = CharField(model_attr='url')
    notes = CharField(model_attr='notes')
    info = CharField(model_attr='info')
"""


class EigensolverIndex(SearchIndex):
    text = CharField(document=True, use_template=True) 
    thePrecision = CharField(model_attr='thePrecision')
    routineName = CharField(model_attr='routineName')
    matrixType = CharField(model_attr='matrixType')
    storageType = CharField(model_attr='storageType')
    url = CharField(model_attr='url')
    notes = CharField(model_attr='notes')
    info = CharField(model_attr='info')


site.register(LinearEquation_simple, LinearEquation_simpleIndex)
site.register(LinearEquation_expert, LinearEquation_expertIndex)
site.register(LinearEquation_driver, LinearEquation_driverIndex)
site.register(LinearLeastSquare, LinearLeastSquareIndex)
#site.register(SymmetricEigenvalue, SymmetricEigenvalueIndex)
#site.register(nonSymmetricEigenvalue, nonSymmetricEigenvalueIndex)
site.register(Eigensolver, EigensolverIndex)




