import datetime
from haystack import indexes

from Computational.models import LinearEquation_computational, LinearEquation_factor, LinearEquation_solve, LinearEquation_condition_number, LinearEquation_error_bound, LinearEquation_invert, LinearEquation_equilibrate
#from Computational.models import Eigensolver_Comput



class LinearEquation_computationalIndex(indexes.SearchIndex, indexes.Indexable):
    text = indexes.CharField(document=True, use_template=True) 
    thePrecision = indexes.CharField(model_attr='thePrecision')
    routineName = indexes.CharField(model_attr='routineName')
    matrixType = indexes.CharField(model_attr='matrixType')
    storageType = indexes.CharField(model_attr='storageType')
    url = indexes.CharField(model_attr='url')
    notes = indexes.CharField(model_attr='notes')
    info = indexes.CharField(model_attr='info')

    def get_model(self):
        return LinearEquation_computational
    
    def index_queryset(self):
        return self.get_model().objects.filter(pub_date__lte=datetime.datetime.now())



class LinearEquation_factorIndex(indexes.SearchIndex, indexes.Indexable):
    text = indexes.CharField(document=True, use_template=True) 
    thePrecision = indexes.CharField(model_attr='thePrecision')
    routineName = indexes.CharField(model_attr='routineName')
    matrixType = indexes.CharField(model_attr='matrixType')
    storageType = indexes.CharField(model_attr='storageType')
    url = indexes.CharField(model_attr='url')
    notes = indexes.CharField(model_attr='notes')
    info = indexes.CharField(model_attr='info')

    def get_model(self):
        return LinearEquation_factor
    
    def index_queryset(self):
        return self.get_model().objects.filter(pub_date__lte=datetime.datetime.now())
    
    

class LinearEquation_solveIndex(indexes.SearchIndex, indexes.Indexable):
    text = indexes.CharField(document=True, use_template=True) 
    thePrecision = indexes.CharField(model_attr='thePrecision')
    routineName = indexes.CharField(model_attr='routineName')
    matrixType = indexes.CharField(model_attr='matrixType')
    storageType = indexes.CharField(model_attr='storageType')
    url = indexes.CharField(model_attr='url')
    notes = indexes.CharField(model_attr='notes')
    info = indexes.CharField(model_attr='info')

    def get_model(self):
        return LinearEquation_solve
    
    def index_queryset(self):
        return self.get_model().objects.filter(pub_date__lte=datetime.datetime.now())
    
    

class LinearEquation_condition_numberIndex(indexes.SearchIndex, indexes.Indexable):
    text = indexes.CharField(document=True, use_template=True) 
    thePrecision = indexes.CharField(model_attr='thePrecision')
    routineName = indexes.CharField(model_attr='routineName')
    matrixType = indexes.CharField(model_attr='matrixType')
    storageType = indexes.CharField(model_attr='storageType')
    url = indexes.CharField(model_attr='url')
    notes = indexes.CharField(model_attr='notes')
    info = indexes.CharField(model_attr='info')

    def get_model(self):
        return LinearEquation_condition_number
    
    def index_queryset(self):
        return self.get_model().objects.filter(pub_date__lte=datetime.datetime.now())
    
    

class LinearEquation_error_boundIndex(indexes.SearchIndex, indexes.Indexable):
    text = indexes.CharField(document=True, use_template=True) 
    thePrecision = indexes.CharField(model_attr='thePrecision')
    routineName = indexes.CharField(model_attr='routineName')
    matrixType = indexes.CharField(model_attr='matrixType')
    storageType = indexes.CharField(model_attr='storageType')
    url = indexes.CharField(model_attr='url')
    notes = indexes.CharField(model_attr='notes')
    info = indexes.CharField(model_attr='info')

    def get_model(self):
        return LinearEquation_error_bound
    
    def index_queryset(self):
        return self.get_model().objects.filter(pub_date__lte=datetime.datetime.now())
    


class LinearEquation_invertIndex(indexes.SearchIndex, indexes.Indexable):
    text = indexes.CharField(document=True, use_template=True) 
    thePrecision = indexes.CharField(model_attr='thePrecision')
    routineName = indexes.CharField(model_attr='routineName')
    matrixType = indexes.CharField(model_attr='matrixType')
    storageType = indexes.CharField(model_attr='storageType')
    url = indexes.CharField(model_attr='url')
    notes = indexes.CharField(model_attr='notes')
    info = indexes.CharField(model_attr='info')

    def get_model(self):
        return LinearEquation_invert
    
    def index_queryset(self):
        return self.get_model().objects.filter(pub_date__lte=datetime.datetime.now())
    
    

class LinearEquation_equilibrateIndex(indexes.SearchIndex, indexes.Indexable):
    text = indexes.CharField(document=True, use_template=True) 
    thePrecision = indexes.CharField(model_attr='thePrecision')
    routineName = indexes.CharField(model_attr='routineName')
    matrixType = indexes.CharField(model_attr='matrixType')
    storageType = indexes.CharField(model_attr='storageType')
    url = indexes.CharField(model_attr='url')
    notes = indexes.CharField(model_attr='notes')
    info = indexes.CharField(model_attr='info')

    def get_model(self):
        return LinearEquation_equilibrate
    
    def index_queryset(self):
        return self.get_model().objects.filter(pub_date__lte=datetime.datetime.now())
    
    
'''
class Eigensolver_ComputIndex(indexes.SearchIndex, indexes.Indexable):
    text = indexes.CharField(document=True, use_template=True) 
    thePrecision = indexes.CharField(model_attr='thePrecision')
    routineName = indexes.CharField(model_attr='routineName')
    matrixType = indexes.CharField(model_attr='matrixType')
    storageType = indexes.CharField(model_attr='storageType')
    url = indexes.CharField(model_attr='url')
    notes = indexes.CharField(model_attr='notes')
    info = indexes.CharField(model_attr='info')
'''







