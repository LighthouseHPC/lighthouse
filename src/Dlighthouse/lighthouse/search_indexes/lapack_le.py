import datetime
from haystack import indexes
from lighthouse.models.lapack_le import lapack_le_driver, lapack_le_computational




class lapack_le_driverIndex(indexes.SearchIndex, indexes.Indexable):
    text = indexes.CharField(document=True, use_template=True) 
    thePrecision = indexes.CharField(model_attr='thePrecision')
    routineName = indexes.CharField(model_attr='routineName')
    matrixType = indexes.CharField(model_attr='matrixType')
    storageType = indexes.CharField(model_attr='storageType')
    url = indexes.CharField(model_attr='url')

    def get_model(self):
        return lapack_le_driver




class lapack_le_computationalIndex(indexes.SearchIndex, indexes.Indexable):
    text = indexes.CharField(document=True, use_template=True) 
    thePrecision = indexes.CharField(model_attr='thePrecision')
    routineName = indexes.CharField(model_attr='routineName')
    matrixType = indexes.CharField(model_attr='matrixType')
    storageType = indexes.CharField(model_attr='storageType')
    url = indexes.CharField(model_attr='url')

    def get_model(self):
        return lapack_le_computational
    
    #def index_queryset(self):
    #    return self.get_model().objects.filter(pub_date__lte=datetime.datetime.now())

