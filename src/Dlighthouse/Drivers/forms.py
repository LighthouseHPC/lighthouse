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
	('LinearEquation_comb',			'Linear Equation, solve A * X = B (square matrix)'),
	('LinearEquation_trans', 		'Linear Equation, solve A**T * X = B or A**H * X = B (square matrix)'),
	('LinearEquation_error_bound',		'Linear Equation with Error Bound'),
	('LinearEquation_factor',		'Matrix Factorization'),
	('LinearEquation_condition_number',	'Condition Number'),
	('LinearEquation_invert',		'Matrix Inverse'),
	('LinearEquation_equilibrate',		'Equilibrate'),		
#	('LinearLeastSquare',	'Linear Least Squares'),
#	('Eigensolver',		'Eigensolver'),
)

class ProblemForm(forms.Form):
	question_prob = forms.ChoiceField(label='What is the type of the problem you would like to solve?', choices=Problem_choices, widget=forms.RadioSelect())





class PrecisionForm(forms.Form):
	question_prec = forms.ChoiceField(label='Would you like to use single or double precision?', choices=([('s','Single'), ('d','Double'), ('what', 'I don\'t know'),]), widget=forms.RadioSelect())





class ComplexForm(forms.Form):
	question_comp = forms.ChoiceField(label='Are there complex numbers in your matrix?', choices=([('y','Yes'), ('n','No'),('what', 'I don\'t know'),]), widget=forms.RadioSelect())  






class MatrixTypeForm(forms.Form):
	question_type = forms.ChoiceField(label='What is the type of your matrix?', choices=[], widget=forms.RadioSelect())
	def __init__(self, request, *args, **kwargs):
		super(MatrixTypeForm, self).__init__(*args, **kwargs)
		self.fields['question_type'].choices = get_model(request.session.get('Application'), request.session.get('Question1')[0]).objects.filter(**request.session['filter_dict']).values_list('matrixType', 'matrixType').distinct()





class StorageForm(forms.Form):
	question_stor = forms.ChoiceField(label='How is your matrix stored?', choices=[], widget=forms.RadioSelect())
	def __init__(self, request, *args, **kwargs):
		super(StorageForm, self).__init__(*args, **kwargs)
		self.fields['question_stor'].choices = get_model(request.session.get('Application'), request.session.get('Question1')[0]).objects.filter(**request.session['filter_dict']).values_list('structureType', 'structureType').distinct()






	
