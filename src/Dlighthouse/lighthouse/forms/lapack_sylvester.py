from django import forms
from django.db.models import get_model
from lighthouse.models.lapack_sylvester import *
from lighthouse.models.lapack_choiceDict import *
#from lighthouse.forms.lapack_eigen import CustomRadioSelect
from django.utils.safestring import mark_safe
    

######-------- For Guided Search --------######
##---- problem form ---- ##
class standardGeneralizedForm(forms.Form):
    sylvester_standardGeneralized = forms.ChoiceField(label='Would you like to solve a standard or generalized Sylvester matrix equation?',
					      widget=forms.RadioSelect(),
					      choices=SYLVESTER_CHOICES
					      )    


    
##---- complex form ----##
class complexNumberForm(forms.Form):
    sylvester_complexNumber = forms.ChoiceField(label='Does your matrix have any complex numbers?',
					      widget=forms.RadioSelect(),
					      choices=NOYES_CHOICES
					      )
    
    

##---- condition form --- ##
class standardConditionForm(forms.Form):
    sylvester_standardCondition = forms.ChoiceField(label=mark_safe('LAPACK only supports upper quasi-triangular matrices A and B in full storage. Do you wish to continue the search?'),
					      widget=forms.RadioSelect(),
					      choices=NOYES_CHOICES
					      )



class generalizedConditionForm(forms.Form):
    sylvester_generalizedCondition = forms.ChoiceField(label=mark_safe('LAPACK only supports upper quasi-triangular matrices A and D in full storage, and upper triangular matrices B and E in full storage. Do you wish to continue the search?'),
					      widget=forms.RadioSelect(),
					      choices=NOYES_CHOICES
					      )



''' matrixTypeForm and storageTypeForm are NOT used '''
###---- matrix type form ----##
#class matrixTypeForm(forms.Form):
#    sylvester_matrixType = forms.ChoiceField(label='What is the type of your matrix?',
#					     widget=CustomRadioSelect(),
#					     choices=[],
#					     initial = u'upper quasi-triangular')
#    def __init__(self, request, *args, **kwargs):
#	super(matrixTypeForm, self).__init__(*args, **kwargs)
#	disableList = []
#	##--- with or without complex numbers, disable the other options ---##
#	if request.session['sylvester_complexNumber'] == 'no' :
#	    self.fields['sylvester_matrixType'].choices = (
#		(u'general',                    	u'general'), 
#		(u'symmetric',                  	u'symmetric'),
#		(u'upper quasi-triangular',		u'upper quasi-triangular'),
#		)
#	else:
#	    self.fields['sylvester_matrixType'].choices = (
#		(u'general',                    	u'general'), 
#		(u'Hermitian',                  	u'Hermitian'),
#		(u'upper quasi-triangular',		u'upper quasi-triangular'),
#		)	    
#	for item in self.fields['sylvester_matrixType'].choices:
#	    if item[1] != u'upper quasi-triangular':5
#		disableList.append(True)
#	    else:
#		disableList.append(False)
#		    
#	self.fields['sylvester_matrixType'].widget.renderer.disable = disableList
#
#
#
#
###---- storage type form ----##
#class storageTypeForm(forms.Form):
#    sylvester_storageType = forms.ChoiceField(label='How is your matrix stored?',
#					      widget=CustomRadioSelect(),
#					      choices=(
#							(u'full',                       u'full'),
#							(u'band',                       u'band'),
#							(u'packed',                     u'packed'),
#							),
#					      initial = u'full')
#    def __init__(self, *args, **kwargs):
#	super(storageTypeForm, self).__init__(*args, **kwargs)
#	disableList = []
#	for item in self.fields['sylvester_storageType'].choices:
#	    if item[1] != u'full':
#		disableList.append(True)
#	    else:
#		disableList.append(False)
#		    
#	self.fields['sylvester_storageType'].widget.renderer.disable = disableList	    

    
    
##--- precision form ---##
class singleDoubleForm(forms.Form):
    sylvester_singleDouble = forms.ChoiceField(label='Would you like to use single or double precision?',
                                              widget=forms.RadioSelect(),
                                              choices=SINGLEDOUBLE_CHOICES
                                              )
    
    
    
    
######-------- For Advanced Search --------######
class advancedForm(forms.Form):
    standard_search = forms.ChoiceField(widget=forms.RadioSelect, choices=NOYES_CHOICES)
    standard_complexNumber = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=NOYES_CHOICES)
    standard_singleDouble = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=SINGLEDOUBLE_CHOICES)
    generalized_search = forms.ChoiceField(widget=forms.RadioSelect, choices=NOYES_CHOICES)
    generalized_complexNumber = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=NOYES_CHOICES)
    generalized_singleDouble = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=SINGLEDOUBLE_CHOICES)    
                                            