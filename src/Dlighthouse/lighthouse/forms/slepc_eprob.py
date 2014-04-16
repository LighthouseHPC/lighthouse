from django import forms
from lighthouse.models.slepc_eprob import *


operation_choices = (
	('Compute eigenvalues',	'Compute eigenvalues'),
	('Something else',	'Something else'),
)



class SlepcGuidedForm(forms.Form):
	#operations = forms.MultipleChoiceField( choices=operation_choices, label='Which of the following operations do you wish to perform?', widget=forms.RadioSelect(attrs={'onclick': "",'id':"main_op"}))
	type = forms.ChoiceField( choices=TYPE_CHOICES, label='What type of matrix do you have?', widget=forms.RadioSelect(attrs={'onclick': "",'id':"main_op"}))
	complex = forms.ChoiceField( choices=EIGEN_YESNO_CHOICES, label='Is your matrix complex?', widget=forms.RadioSelect(attrs={'onclick': "",'id':"main_op"}),required=False)
	size = forms.IntegerField(label='Enter approximate size of the matrix.',required=False)
	numEigenvalues = forms.IntegerField(label='How many eigenvalues do you want?',required=False)
	spectrum = forms.ChoiceField( choices=SPECTRUM_CHOICES, label='Select the desired portion of spectrum', widget=forms.RadioSelect(attrs={'onclick': "",'id':"main_op"}),required=False)
	tolerance = forms.DecimalField(label='Enter the desired tolerance for the residual.',required=False)
	processors = forms.IntegerField(label='Enter the number of processors available to run the solver.', min_value=1,required=False)
	precision = forms.ChoiceField( choices=PRECISION_CHOICES, label='Select your precision preference.', widget=forms.RadioSelect(attrs={'onclick': "",'id':"main_op"}),required=False)
