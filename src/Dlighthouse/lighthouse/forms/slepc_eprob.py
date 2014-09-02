from django import forms
from lighthouse.models.slepc_eprob import *



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


