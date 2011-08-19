from django import forms
from django.db.models import get_model
from Drivers.models import LinearEquation, LinearLeastSquare, Eigensolver



'''
class LinearEquationForm(forms.ModelForm):    
    class Meta:
        model = LinearEquation


class LinearLeastSquareForm(forms.ModelForm):    
    class Meta:
        model = LinearLeastSquare


class EigensolverForm(forms.ModelForm):    
    class Meta:
        model = Eigensolver
'''



Problem_choices = (
	('LinearEquation', 'Linear Equation, solve Ax = b (square matrix)'),
	('LinearLeastSquare', 'Linear Least Squares'),
	('Eigensolver', 'Eigensolver'),
)

class ProblemForm(forms.Form):
	question_prob = forms.ChoiceField(label='What is the type of the problem you would like to solve?', choices=Problem_choices, widget=forms.RadioSelect())





class PrecisionForm(forms.Form):
	question_prec = forms.ChoiceField(label='Would you like to use single or double precision?', choices=([('s','Single'), ('d','Double'), ('what', 'I don\'t know'),]), widget=forms.RadioSelect())





class ComplexForm(forms.Form):
	question_comp = forms.ChoiceField(label='Are there complex numbers in your matrix?', choices=([('y','Yes'), ('n','No'),('what', 'I don\'t know'),]), widget=forms.RadioSelect())  





#MatrixType_Choices = (
#	('general', 'General'),
#       ('symmetric', 'Symmetric'),
#       ('Hermitian', 'Hermitian'),
#       ('SPD', 'SPD (symmetric and positive definite)'),
#	('what', 'I don\'t know'),
#)

class MatrixTypeForm(forms.Form):
	question_type = forms.ChoiceField(label='What is the type of your matrix?', choices=[], widget=forms.RadioSelect())
	def __init__(self, request, *args, **kwargs):
		super(MatrixTypeForm, self).__init__(*args, **kwargs)
		self.fields['question_type'].choices = get_model('Drivers', request.session.get('Question1')[0]).objects.filter(**request.session['filter_dict']).values_list('matrixType', 'matrixType').distinct()




'''
ZeroStructure_Choices = (
	('den', 'Dense'),
	('ban', 'Banded'),
	('tri', 'Tridiagonal'),
	('what', 'I don\'t know'),
)

class ZeroStructureForm(forms.Form):
	question_zero = forms.ChoiceField(choices=ZeroStructure_Choices, label='What is the zero structure of your matrix?', widget=forms.RadioSelect())
'''




#StorageType_Choices = (
#	('full', 'Full'),
#	('banded', 'Banded'),
#	('packed', 'Packed'),
#	('tridiagonal', 'Tridiagonal'),
#	('what', 'I don\'t know'),
#)  	


class StorageForm(forms.Form):
	question_stor = forms.ChoiceField(label='How is your matrix stored?', choices=[], widget=forms.RadioSelect())
	def __init__(self, request, *args, **kwargs):
		super(StorageForm, self).__init__(*args, **kwargs)
		self.fields['question_stor'].choices = get_model('Drivers', request.session.get('Question1')[0]).objects.filter(**request.session['filter_dict']).values_list('structureType', 'structureType').distinct()






	
