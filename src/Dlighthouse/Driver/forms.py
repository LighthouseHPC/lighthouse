from django import forms
from django.db.models import get_model





### ---------------- for simple search ---------------- ###

Problem_choices = (
	('Combine LinearEquation_only solve',					'Solve a linear equation only'),
	('Computational LinearEquation_computational factor',			'Factor a matrix (PA = LU)'),
	('Computational LinearEquation_computational refine',			'Refine the solution to a linear system'),
	('Computational LinearEquation_computational error',			'Compute forward or backward error bounds for the solution to a linear system'),
	('Computational LinearEquation_computational condition', 		'Estimate the condition number of a matrix'),
	('Computational LinearEquation_computational equilibrate',		'Equilibrate a matrix'),
	('Computational LinearEquation_computational inverse',			'Invert a matrix using provided factors (P, L, U)'),
	('Driver LinearEquation_expert',					'Solve a linear equation AND'),
	('Driver LinearEquation_expert refine',					'Refine the solution'),
	('Driver LinearEquation_expert error',					'Compute forward or backward error bounds for the solution'),
	('Driver LinearEquation_expert condition', 				'Estimate the condition number of a matrix'),
	('Driver LinearEquation_expert equilibrate',				'Equilibrate a matrix'),
)



class ProblemForm(forms.Form):
	question_prob = forms.MultipleChoiceField(label='Which of the following functions do you wish to execute?', widget=forms.CheckboxSelectMultiple(), choices=Problem_choices)

	@staticmethod
	def find(answer):
		for item in Problem_choices:
			if answer == item[0]:
				return item[1]




Equation_choices = (
	('original',				'AX = B'),
	('transpose',				'A<sup>T</sup>X = B'),
	('Hermitian_trans',			'A<sup>H</sup>X = B'),
)


class EquationForm(forms.Form):
	question_equa = forms.ChoiceField(label='What form of the linear system do you want to solve?', widget=forms.RadioSelect(), choices=Equation_choices)




class FactorForm(forms.Form):
	question_fact = forms.ChoiceField(label='Is your matrix factored?', choices=([('y','yes'), ('n','no')]), widget=forms.RadioSelect(), initial = dict())





class ComplexForm(forms.Form):
	question_comp = forms.ChoiceField(label='Are there complex numbers in your matrix?', choices=([('y','yes'), ('n','no'),('what', 'I don\'t know'),]), widget=forms.RadioSelect(attrs={'onclick': "disableComplex(request.session['Complex_initial'])"}), initial = dict())





class MatrixTypeForm(forms.Form):
	question_type = forms.ChoiceField(label='What is the type of your matrix?', choices=[], widget=forms.RadioSelect())
	def __init__(self, request, *args, **kwargs):
		super(MatrixTypeForm, self).__init__(*args, **kwargs)
		self.fields['question_type'].choices = request.session['Routines'].values_list('matrixType', 'matrixType').distinct()
		if len(self.fields['question_type'].choices) == 1:
			self.fields['question_type'].initial = self.fields['question_type'].choices[0][1]




class StorageForm(forms.Form):
	question_stor = forms.ChoiceField(label='How is your matrix stored?', choices=[], widget=forms.RadioSelect())
	def __init__(self, request, *args, **kwargs):
		super(StorageForm, self).__init__(*args, **kwargs)
		self.fields['question_stor'].choices = request.session['Routines'].values_list('storageType', 'storageType').distinct()
		if len(self.fields['question_stor'].choices) == 1:
			self.fields['question_stor'].initial = self.fields['question_stor'].choices[0][1]




class PrecisionForm(forms.Form):
	question_prec = forms.ChoiceField(label='Would you like to use single or double precision?', choices=([('s','single'), ('d','double'), ('what', 'I don\'t know'),]), widget=forms.RadioSelect())










### ---------------- for advanced search ---------------- ###
Advanced_choices = (
	('Computational LinearEquation_computationalForm', 'Computational Routines'),
	('Driver LinearEquation_simpleForm', 'Simple Driver Routines'),
	('Driver LinearEquation_expertForm', 'Expert Driver Routines'),
)

class AdvancedForm(forms.Form):
	advanced = forms.MultipleChoiceField(label='Which of the following routine categories would you like to search?', required=True, widget=forms.CheckboxSelectMultiple(), choices=Advanced_choices)

	@staticmethod
	def find(answer):
		for item in Advanced_choices:
			if answer == item[0]:
				return item[1]


Function_choices = (
	('notes factor',			'Factor a matrix (PA = LU)'),
	('notes error',				'Compute forward or backward error bounds for the solution to a linear system'),
	('notes refine',			'Refine the solution to a linear system'),
	('notes condition', 			'Estimate the condition number of a matrix'),
	('notes equilibrate',			'Equilibrate a matrix'),
	('notes inverse',			'Invert a matrix using provided factors (P, L, U)'),
	('notes solve',				'Solve a linear system using provided factors (P, L, U)'), 
)

Complex_choices = (
	('complex yes',				'Yes'),
	('complex no',				'No'),
)

MatrixTypeComputational_choices = (
	('matrixType general',			'General'),
	('matrixType symmetric',		'Symmetric'),
	('matrixType SPD',			'SPD'),	
	('matrixType Hermitian',		'Hermitian'),	
	('matrixType HPD',			'HPD'),	
	('matrixType triangular',		'Triangular'),	
)

MatrixType_choices = (
	('matrixType general',			'General'),
	('matrixType symmetric',		'Symmetric'),
	('matrixType SPD',			'SPD'),	
	('matrixType Hermitian',		'Hermitian'),	
	('matrixType HPD',			'HPD'),		
)

StorageType_choices = (
	('storageType full',			'Full'),
	('storageType band',			'band'),
	('storageType packed',			'Packed'),
	('storageType tridiagonal',		'Tridiagonal'),
)

Precision_choices = (
	('thePrecision single',			'Single'),
	('thePrecision double',			'Double'),
)



###--------------- Linear Equation Computational Forms ------------------###
class LinearEquation_computationalForm(forms.Form):
	Description = 'Perform various computational tasks listed as follows:'
	Function = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=Function_choices)
	LinearEquation_computationalComplex = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=Complex_choices)
	MatrixType = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=MatrixTypeComputational_choices)
	StorageType = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=StorageType_choices)
	Precision = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=Precision_choices)


###--------------- Linear Equation Simple Driver Forms ------------------###
class LinearEquation_simpleForm(forms.Form):
	Description = 'Solve a system of linear equations'
	Function = 'AX = B'
	LinearEquation_simpleComplex = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=Complex_choices)
	MatrixType = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=MatrixType_choices)
	StorageType = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=StorageType_choices)
	Precision = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=Precision_choices)


###--------------- Linear Equation Expert Driver Forms ------------------###
ExpertDriverEquation_choices = (
	('Driver LinearEquation_expert notes original',					'AX = B'),
	('Driver LinearEquation_expert notes transpose',				'A<sup>T</sup>X = B'),
	('Driver LinearEquation_expert notes Hermitian_trans',				'A<sup>H</sup>X = B'),
)

ExpertDriverFunction_choices = (
	('Driver LinearEquation_expert notes condition',				'Estimate the matrix condition number'),
	('Driver LinearEquation_expert notes error',					'Compute error bounds for the solution'),
	('Driver LinearEquation_expert notes refine',					'Refine the solution'),
	('Driver LinearEquation_expert notes equilibrate',				'Equilibrate the matrix'),
)

class LinearEquation_expertForm(forms.Form):
	Description = forms.MultipleChoiceField(label='Solve a system of linear equations', widget=forms.CheckboxSelectMultiple(), choices=ExpertDriverEquation_choices)
	Function = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=ExpertDriverFunction_choices)
	Complex = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=Complex_choices)	
	MatrixType = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=MatrixType_choices)	
	StorageType = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=StorageType_choices)	
	Precision = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=Precision_choices)


