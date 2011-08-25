import datetime
from haystack.indexes import *
from haystack import site
from Computational.models import Eigensolver_Comput




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



site.register(Eigensolver_Comput, Eigensolver_ComputIndex)




