from django import forms
from lighthouse.models.slepc_eprob import *

###
# For Guided Search
###

class problemClassForm(forms.Form):
	problemClass = forms.ChoiceField(label='Which of the following do you wish to compute?',
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

class miscFormPEP(forms.Form):
	complex = forms.ChoiceField( choices=EIGEN_YESNO_CHOICES, label='Is your matrix complex?', widget=forms.RadioSelect(attrs={'onclick': "",'id':"complexid"}),required=False)
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
# For NEP
#
class definitionFormNEP(forms.Form):
	definitionNEP = forms.ChoiceField(label='How is your problem defined?',
		widget=forms.RadioSelect(),
		choices=NEP_DEFINITION
		)
class numEPFormNEP(forms.Form):
	numEPNEP = forms.ChoiceField(label = 'Would you like to compute more than one eigenpair?',
		widget=forms.RadioSelect(),
		choices=EIGEN_YESNO_CHOICES
		)
class miscForm1NEP(forms.Form):
	complex = forms.ChoiceField( choices=EIGEN_YESNO_CHOICES, label='Is your matrix complex?', widget=forms.RadioSelect(attrs={'onclick': "",'id':"complexid"}),required=False)
	size = forms.IntegerField(label='Enter approximate size of the matrix.',required=False)
	spectrum = forms.ChoiceField( choices=SPECTRUM_CHOICES, label='Select the desired portion of spectrum', widget=forms.RadioSelect(attrs={'onclick': "",'id':"spectrumid"}),required=True)
	tolerance = forms.DecimalField(label='Enter the desired tolerance for the residual.',required=False, initial=0.0001)
	processors = forms.IntegerField(label='Enter the number of processors available to run the solver.', min_value=1,required=False)
	precision = forms.ChoiceField( choices=PRECISION_CHOICES, label='Select your precision preference.', widget=forms.RadioSelect(attrs={'onclick': "",'id':"precisionid"}),required=False)

class miscForm2NEP(forms.Form):
	complex = forms.ChoiceField( choices=EIGEN_YESNO_CHOICES, label='Is your matrix complex?', widget=forms.RadioSelect(attrs={'onclick': "",'id':"complexid"}),required=False)
	size = forms.IntegerField(label='Enter approximate size of the matrix.',required=False)
	numEigenvalues = forms.IntegerField(label='How many eigenvalues do you want?',required=False)
	spectrum = forms.ChoiceField( choices=SPECTRUM_CHOICES, label='Select the desired portion of spectrum', widget=forms.RadioSelect(attrs={'onclick': "",'id':"spectrumid"}),required=True)
	tolerance = forms.DecimalField(label='Enter the desired tolerance for the residual.',required=False, initial=0.0001)
	processors = forms.IntegerField(label='Enter the number of processors available to run the solver.', min_value=1,required=False)
	precision = forms.ChoiceField( choices=PRECISION_CHOICES, label='Select your precision preference.', widget=forms.RadioSelect(attrs={'onclick': "",'id':"precisionid"}),required=False)

#
# End of NEP
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