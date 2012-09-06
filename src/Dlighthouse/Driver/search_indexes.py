import datetime
from haystack import indexes

from Driver.models import LinearEquation_simple, LinearEquation_expert
#from Driver.models import LinearLeastSquare
#from Driver.models import Eigensolver
#from Driver.models import SymmetricEigenvalue
#from Driver.models import nonSymmetricEigenvalue



class LinearEquation_simpleIndex(indexes.SearchIndex, indexes.Indexable):
    text = indexes.CharField(document=True, use_template=True) 
    thePrecision = indexes.CharField(model_attr='thePrecision')
    routineName = indexes.CharField(model_attr='routineName')
    matrixType = indexes.CharField(model_attr='matrixType')
    storageType = indexes.CharField(model_attr='storageType')
    url = indexes.CharField(model_attr='url')
    notes = indexes.CharField(model_attr='notes')
    info = indexes.CharField(model_attr='info')

    def get_model(self):
        return LinearEquation_simple
    
    def index_queryset(self):
        return self.get_model().objects.filter(pub_date__lte=datetime.datetime.now())



class LinearEquation_expertIndex(indexes.SearchIndex, indexes.Indexable):
    text = indexes.CharField(document=True, use_template=True) 
    thePrecision = indexes.CharField(model_attr='thePrecision')
    routineName = indexes.CharField(model_attr='routineName')
    matrixType = indexes.CharField(model_attr='matrixType')
    storageType = indexes.CharField(model_attr='storageType')
    url = indexes.CharField(model_attr='url')
    notes = indexes.CharField(model_attr='notes')
    info = indexes.CharField(model_attr='info')

    def get_model(self):
        return LinearEquation_expert
    
    def index_queryset(self):
        return self.get_model().objects.filter(pub_date__lte=datetime.datetime.now())



'''
class LinearLeastSquareIndex(indexes.SearchIndex, indexes.Indexable):
    text = indexes.CharField(document=True, use_template=True) 
    thePrecision = indexes.CharField(model_attr='thePrecision')
    routineName = indexes.CharField(model_attr='routineName')
    matrixType = indexes.CharField(model_attr='matrixType')
    storageType = indexes.CharField(model_attr='storageType')
    url = indexes.CharField(model_attr='url')
    notes = indexes.CharField(model_attr='notes')
    info = indexes.CharField(model_attr='info')



class SymmetricEigenvalueIndex(indexes.SearchIndex, indexes.Indexable):
    text = indexes.CharField(document=True, use_template=True) 
    thePrecision = indexes.CharField(model_attr='thePrecision')
    routineName = indexes.CharField(model_attr='routineName')
    matrixType = indexes.CharField(model_attr='matrixType')
    storageType = indexes.CharField(model_attr='storageType')
    url = indexes.CharField(model_attr='url')
    notes = indexes.CharField(model_attr='notes')
    info = indexes.CharField(model_attr='info')



class nonSymmetricEigenvalueIndex(indexes.SearchIndex, indexes.Indexable):
    text = indexes.CharField(document=True, use_template=True) 
    thePrecision = indexes.CharField(model_attr='thePrecision')
    routineName = indexes.CharField(model_attr='routineName')
    matrixType = indexes.CharField(model_attr='matrixType')
    storageType = indexes.CharField(model_attr='storageType')
    url = indexes.CharField(model_attr='url')
    notes = indexes.CharField(model_attr='notes')
    info = indexes.CharField(model_attr='info')



class EigensolverIndex(indexes.SearchIndex, indexes.Indexable):
    text = indexes.CharField(document=True, use_template=True) 
    thePrecision = indexes.CharField(model_attr='thePrecision')
    routineName = indexes.CharField(model_attr='routineName')
    matrixType = indexes.CharField(model_attr='matrixType')
    storageType = indexes.CharField(model_attr='storageType')
    url = indexes.CharField(model_attr='url')
    notes = indexes.CharField(model_attr='notes')
    info = indexes.CharField(model_attr='info')
'''




