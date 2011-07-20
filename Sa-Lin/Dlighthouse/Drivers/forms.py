from django import forms

Problem_choices = (
	('LinearEquation', 'Linear Equation, solve Ax = b (square matrix)'),
	('LinearLeastSquare', 'Linear Least Squares'),
	('Eigensolver', 'Eigensolver'),
	('what', 'I don\'t know'),
)

class ProblemForm(forms.Form):
	question_prob = forms.ChoiceField(choices=Problem_choices, label='What is the type of the problem you would like to solve?', widget=forms.RadioSelect())





class PrecisionForm(forms.Form):
	question_prec = forms.ChoiceField(choices=([('s','Single'), ('d','Double'), ('what', 'I don\'t know'),]), label='Would you like to use single or double precision?', widget=forms.RadioSelect())





class ComplexForm(forms.Form):
	question_comp = forms.ChoiceField(choices=([('y','Yes'), ('n','No'),('what', 'I don\'t know'),]), label='Are there complex numbers in your matrix?', widget=forms.RadioSelect())  





MatrixType_Choices = (
	('general', 'General'),
        ('symmetric', 'Symmetric'),
        ('Hermitian', 'Hermitian'),
        ('SPD', 'SPD (symmetric and positive definite)'),
	('what', 'I don\'t know'),
)

class MatrixTypeForm(forms.Form):
	question_type = forms.ChoiceField(choices=MatrixType_Choices, label='What is the type of your matrix?', widget=forms.RadioSelect())





ZeroStructure_Choices = (
	('den', 'Dense'),
	('ban', 'Banded'),
	('tri', 'Tridiagonal'),
	('what', 'I don\'t know'),
)

class ZeroStructureForm(forms.Form):
	question_zero = forms.ChoiceField(choices=ZeroStructure_Choices, label='What is the zero structure of your matrix?', widget=forms.RadioSelect())





StorageType_Choices = (
	('full', 'Full'),
	('banded', 'Banded'),
	('packed', 'Packed'),
	('tridiagonal', 'Tridiagonal'),
	('what', 'I don\'t know'),
)  	


class StorageForm(forms.Form):
	question_stor = forms.ChoiceField(choices=StorageType_Choices, label='How is your matrix stored?', widget=forms.RadioSelect())






	
