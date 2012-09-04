from django import forms
from django.db.models import get_model





### ---------------- for simple search ---------------- ###

Problem_choices = (
	('Combine LinearEquation_only solve',					'Solve a system of linear equations only'),
	('Computational LinearEquation_factor factorization',			'Factor a matrix'),
	('Computational LinearEquation_error_bound refine',			'Refine the solution to a linear system'),
	('Computational LinearEquation_error_bound error',			'Compute forward or backward error bound for the solution to a linear system'),
	('Computational LinearEquation_condition_number condition', 		'Estimate the condition number of a matrix'),
	('Computational LinearEquation_equilibrate equilibrate',		'Equilibrate a matrix'),
	('Computational LinearEquation_invert inverse',				'Invert a matrix using provided factors (P, L, U)'),
	('Driver LinearEquation_expert',					'Solve a system of linear equations AND'),
	('Driver LinearEquation_expert refine',					'Refine the solution'),
	('Driver LinearEquation_expert error',					'Compute forward or backward error bounds for the solution'),
	('Driver LinearEquation_expert condition', 				'Estimate the condition number of the matrix'),
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




###--------------- Linear Equation Computational Forms ------------------###
Function_choices = (
	('factor',			'factor a matrix (PA = LU)'),
	('error',			'compute forward or backward error bounds for the solution to a linear system'),
	('refine',			'refine the solution to a linear system'),
	('condition', 			'estimate the condition number of a matrix'),
	('equilibrate',			'equilibrate a matrix'),
	('inverse',			'invert a matrix using provided factors (P, L, U)'),
	('solve',			'solve a linear system using provided factors (P, L, U)'), 
)

Complex_choices = (
	('yes',				'yes'),
	('no',				'no'),
)

MatrixTypeComputational_choices = (
	('general',			'general'),
	('symmetric',			'symmetric'),
	('SPD',				'SPD'),	
	('Hermitian',			'Hermitian'),	
	('HPD',				'HPD'),	
	('triangular',			'triangular'),	
)

MatrixType_choices = (
	('general',			'general'),
	('symmetric',			'symmetric'),
	('SPD',				'SPD'),	
	('Hermitian',			'Hermitian'),	
	('HPD',				'HPD'),		
)

StorageType_choices = (
	('full',			'full'),
	('band',			'band'),
	('packed',			'packed'),
	('tridiagonal',			'tridiagonal'),
)

Precision_choices = (
	('single',			'single'),
	('double',			'double'),
)




class LinearEquation_computationalForm(forms.Form):
	Description = 'Perform various computational tasks'
	LinearEquation_computationalFunction = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=Function_choices)
	LinearEquation_computationalComplex = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=Complex_choices)
	LinearEquation_computationalMatrixType = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=MatrixTypeComputational_choices)
	LinearEquation_computationalStorageType = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=StorageType_choices)
	LinearEquation_computationalPrecision = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=Precision_choices)

	@staticmethod
	def find(answer):
		for item in Function_choices:
			if answer == item[0]:
				return item[1]




###--------------- Linear Equation Simple Driver Forms ------------------###
class LinearEquation_simpleForm(forms.Form):
	Description = 'Solve a system of linear equations'
	LinearEquation_simpleFunction = forms.MultipleChoiceField(choices=([('solve','AX=B')]), widget=forms.CheckboxSelectMultiple(), initial=['solve'])
	LinearEquation_simpleComplex = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=Complex_choices)
	LinearEquation_simpleMatrixType = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=MatrixType_choices)
	LinearEquation_simpleStorageType = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=StorageType_choices)
	LinearEquation_simplePrecision = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=Precision_choices)

	@staticmethod
	def find(answer):
		return 'AX=B'





###--------------- Linear Equation Expert Driver Forms ------------------###
ExpertDriverEquation_choices = (
	('solve',					'AX = B'),
	('transpose',					'A<sup>T</sup>X = B'),
	('Hermitian_trans',				'A<sup>H</sup>X = B'),
)

ExpertDriverFunction_choices = (
	('condition',					'estimate the matrix condition number'),
	('error',					'compute error bounds for the solution'),
	('refine',					'refine the solution'),
	('equilibrate',					'equilibrate the matrix'),
)	

class LinearEquation_expertForm(forms.Form):
	Description = 'Solve a system of linear equations AND perform various computational routines'
	LinearEquation_expertEquation = forms.MultipleChoiceField(label='Equations:', widget=forms.CheckboxSelectMultiple(), choices=ExpertDriverEquation_choices)
	LinearEquation_expertFunction = forms.MultipleChoiceField(label='Tasks:', widget=forms.CheckboxSelectMultiple(), choices=ExpertDriverFunction_choices)
	LinearEquation_expertComplex = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=Complex_choices)	
	LinearEquation_expertMatrixType = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=MatrixType_choices)	
	LinearEquation_expertStorageType = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=StorageType_choices)	
	LinearEquation_expertPrecision = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=Precision_choices)

	@staticmethod
	def find_equation(answer):
		for item in ExpertDriverEquation_choices:
			if answer == item[0]:
				return item[1]

	@staticmethod
	def find_function(answer):
		for item in ExpertDriverFunction_choices:
			if answer == item[0]:
				return item[1]




###--------------- for the script area ------------------###
class scriptForm(forms.Form):
	#script = forms.CharField(widget=forms.EditorInput(attrs={'rows':'15','cols':'60','plugins':"[]"}))
	script = forms.CharField(widget=forms.Textarea(attrs={'dojoType':'dijit.form.SimpleTextarea','rows':'22', 'cols':'60',
							      'styleSheets':"/dojotoolkit/dojo/resources/dojo.css"}))


