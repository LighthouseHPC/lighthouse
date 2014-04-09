from django import forms
from django.db.models import get_model
from lighthouse.models.lapack_eigen import *


noyes_choices = (
    ('no',	'No'),
    ('yes',	'Yes'),
    )




###-------- For Guided Search --------###
##---- problem form ---- ##
Problem_choices = (
	('standard',			u'Standard eigenproblem'),
	('generalized', 		u'Generalized eigenproblem'),
	('sylvester',			u'Sylvester matrix equation'),
	('svd',				u'Singular value decomposition'),
)

class problemForm(forms.Form):
    eigen_prob = forms.ChoiceField(label='Which of the following problems would you like to solve?',
					      widget=forms.RadioSelect(),
					      choices=Problem_choices
					      )    


##---- complex form ----##

class complexForm(forms.Form):
    eigen_complex = forms.ChoiceField(label='Does your matrix have any complex numbers?',
					      widget=forms.RadioSelect(),
					      choices=noyes_choices
					      )


##---- matrix type form ----##
class matrixTypeForm(forms.Form):
        eigen_matrixType = forms.ChoiceField(label='What is the type of your matrix?', choices=[], widget=forms.RadioSelect())
        def __init__(self, request, *args, **kwargs):
                super(matrixTypeForm, self).__init__(*args, **kwargs)
		self.fields['eigen_matrixType'].choices = request.session['Routines'].values_list('matrixType', 'matrixType').distinct()



##---- storage type form ----##
class storageTypeForm(forms.Form):
        eigen_storageType = forms.ChoiceField(label='How is your matrix stored?', choices=[], widget=forms.RadioSelect())
        def __init__(self, request, *args, **kwargs):
                super(storageTypeForm, self).__init__(*args, **kwargs)
		self.fields['eigen_storageType'].choices = request.session['Routines'].values_list('storageType', 'storageType').distinct()
		if len(self.fields['eigen_storageType'].choices) == 1:
                        self.fields['eigen_storageType'].initial = self.fields['eigen_storageType'].choices[0][1]



##--- selected eigenvalue form ---##
class selectedEVForm(forms.Form):
    eigen_slectedEV = forms.ChoiceField(label='Do you only need eigenvalues within a specific range?',
					      widget=forms.RadioSelect(),
					      choices=noyes_choices
					      )