import datetime
from haystack import indexes
from models import least



class leastIndex(indexes.SearchIndex, indexes.Indexable):
    text = indexes.CharField(document=True, use_template=True) 
    thePrecision = indexes.CharField(model_attr='thePrecision')
    routineName = indexes.CharField(model_attr='routineName')
    standardGeneralized = indexes.CharField(model_attr='standardGeneralized')
    svd = indexes.CharField(model_attr='svd')
    qr = indexes.CharField(model_attr='qr')
    url = indexes.CharField(model_attr='url')

    def get_model(self):
        return least


    
    #def index_queryset(self):
    #    return self.get_model().objects.filter(pub_date__lte=datetime.datetime.now())

