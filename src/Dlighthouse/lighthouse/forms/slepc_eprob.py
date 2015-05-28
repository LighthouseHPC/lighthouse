from django import forms
from lighthouse.models.slepc_eprob import *



###
# For Guided Search
###

class problemClassForm(forms.Form):
	problemClassPEP = forms.ChoiceField(label='Which of the following do you wish to compute?',
		widget=forms.RadioSelect(),
		choices=PROBLEMCLASS_CHOICE
		)

###
# For PEP
#

class polynomialDegreeFormPEP(forms.Form):
	polynomialDegreePEP = forms.ChoiceField(label='What is the degree of your polynomial?',
		widget=forms.RadioSelect(),
		choices=PEP_POLYNOMIAL_DEGREE
		)
class problemTypeFormPEP(forms.Form):
	problemTypePEP = forms.ChoiceField(label='What type of problem do you have?',
		widget=forms.RadioSelect(),
		choices=PEP_PROBLEM_TYPE
		)

class miscPEP(forms.Form):
	size = forms.IntegerField(label='Enter approximate size of the matrix.',required=False)
	numEigenvalues = forms.IntegerField(label='How many eigenvalues do you want?',required=False)
	spectrum = forms.ChoiceField( choices=SPECTRUM_CHOICES, label='Select the desired portion of spectrum', widget=forms.RadioSelect(attrs={'onclick': "",'id':"spectrumid"}),required=True)
	tolerance = forms.DecimalField(label='Enter the desired tolerance for the residual.',required=False, initial=0.0001)
	processors = forms.IntegerField(label='Enter the number of processors available to run the solver.', min_value=1,required=False)
	precision = forms.ChoiceField( choices=PRECISION_CHOICES, label='Select your precision preference.', widget=forms.RadioSelect(attrs={'onclick': "",'id':"precisionid"}),required=False)

#
# End of PEP
###

###
#For EPS (Legacy)
#
class SlepcGuidedForm(forms.Form):
	type = forms.ChoiceField( choices=TYPE_CHOICES, label='What type of matrix do you have?', widget=forms.RadioSelect(attrs={'onclick': "",'id':"typeid"}))
	complex = forms.ChoiceField( choices=EIGEN_YESNO_CHOICES, label='Is your matrix complex?', widget=forms.RadioSelect(attrs={'onclick': "",'id':"complexid"}),required=False)
	binary = forms.ChoiceField( choices=EIGEN_YESNO_CHOICES, label='Is your matrix binary?', widget=forms.RadioSelect(attrs={'onclick': "",'id':"binaryid"}),required=False)
	size = forms.IntegerField(label='Enter approximate size of the matrix.',required=False)
	numEigenvalues = forms.IntegerField(label='How many eigenvalues do you want?',required=False)
	spectrum = forms.ChoiceField( choices=SPECTRUM_CHOICES, label='Select the desired portion of spectrum', widget=forms.RadioSelect(attrs={'onclick': "",'id':"spectrumid"}),required=True)
	tolerance = forms.DecimalField(label='Enter the desired tolerance for the residual.',required=False, initial=0.0001)
	processors = forms.IntegerField(label='Enter the number of processors available to run the solver.', min_value=1,required=False)
	precision = forms.ChoiceField( choices=PRECISION_CHOICES, label='Select your precision preference.', widget=forms.RadioSelect(attrs={'onclick': "",'id':"precisionid"}),required=False)

#
# End of EPS
###