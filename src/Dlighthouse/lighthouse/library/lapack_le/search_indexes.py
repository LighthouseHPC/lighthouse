import datetime
from haystack import indexes

from lapack.models import LinearEquation_driver, LinearEquation_computational




class LinearEquation_driverIndex(indexes.RealTimeSearchIndex, indexes.Indexable):
    text = indexes.CharField(document=True, use_template=True) 
    thePrecision = indexes.CharField(model_attr='thePrecision')
    routineName = indexes.CharField(model_attr='routineName')
    matrixType = indexes.CharField(model_attr='matrixType')
    storageType = indexes.CharField(model_attr='storageType')

    def get_model(self):
        return LinearEquation_driver
    
    #def index_queryset(self):
    #    return self.get_model().objects.filter(pub_date__lte=datetime.datetime.now())



class LinearEquation_computationalIndex(indexes.RealTimeSearchIndex, indexes.Indexable):
    text = indexes.CharField(document=True, use_template=True) 
    thePrecision = indexes.CharField(model_attr='thePrecision')
    routineName = indexes.CharField(model_attr='routineName')
    matrixType = indexes.CharField(model_attr='matrixType')
    storageType = indexes.CharField(model_attr='storageType')

    def get_model(self):
        return LinearEquation_computational
    
    #def index_queryset(self):
    #    return self.get_model().objects.filter(pub_date__lte=datetime.datetime.now())

