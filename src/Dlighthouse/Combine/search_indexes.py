import datetime
from haystack.indexes import *
from haystack import site
from Combine.models import LinearEquation_comb, LinearEquation_trans



class LinearEquation_combIndex(SearchIndex):
    text = CharField(document=True, use_template=True) 
    thePrecision = CharField(model_attr='thePrecision')
    routineName = CharField(model_attr='routineName')
    matrixType = CharField(model_attr='matrixType')
    structureType = CharField(model_attr='structureType')
    url = CharField(model_attr='url')
    problem = CharField(model_attr='problem')
    description = CharField(model_attr='description')
    info = CharField(model_attr='info')



class LinearEquation_transIndex(SearchIndex):
    text = CharField(document=True, use_template=True) 
    thePrecision = CharField(model_attr='thePrecision')
    routineName = CharField(model_attr='routineName')
    matrixType = CharField(model_attr='matrixType')
    structureType = CharField(model_attr='structureType')
    url = CharField(model_attr='url')
    problem = CharField(model_attr='problem')
    description = CharField(model_attr='description')
    info = CharField(model_attr='info')


site.register(LinearEquation_comb, LinearEquation_combIndex)
site.register(LinearEquation_trans, LinearEquation_transIndex)




