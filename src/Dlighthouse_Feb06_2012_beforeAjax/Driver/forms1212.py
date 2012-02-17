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
	('Driver LinearEquation_simple complex yes',					'Yes'),
	('Driver LinearEquation_simple complex no',					'No'),
	('Driver LinearEquation_simple matrixType general',				'General'),
	('Driver LinearEquation_simple matrixType symmetric',				'Symmetric'),
	('Driver LinearEquation_simple matrixType SPD',					'SPD'),	
	('Driver LinearEquation_simple matrixType Hermitian',				'Hermitian'),	
	('Driver LinearEquation_simple matrixType HPD',					'HPD'),	
	('Driver LinearEquation_simple storageType full',				'Full'),
	('Driver LinearEquation_simple storageType band',				'band'),
	('Driver LinearEquation_simple storageType packed',				'Packed'),
	('Driver LinearEquation_simple storageType tridiagonal',			'Tridiagonal'),
	('Driver LinearEquation_simple thePrecision single',				'Single'),
	('Driver LinearEquation_simple thePrecision double',				'Double'),
	

	('Computational LinearEquation_computational notes factor',			'Factor a matrix (PA = LU)'),
	('Computational LinearEquation_computational notes error',			'Compute forward or backward error bounds for the solution to a linear system'),
	('Computational LinearEquation_computational notes refine',			'Refine the solution to a linear system'),
	('Computational LinearEquation_computational notes condition', 			'Estimate the condition number of a matrix'),
	('Computational LinearEquation_computational notes equilibrate',		'Equilibrate a matrix'),
	('Computational LinearEquation_computational notes inverse',			'Invert a matrix using provided factors (P, L, U)'),
	('Computational LinearEquation_computational notes solve',			'Solve a linear system using provided factors (P, L, U)'), 
	('Computational LinearEquation_computational complex yes',			'Yes'),
	('Computational LinearEquation_computational complex no',			'No'),
	('Computational LinearEquation_computational matrixType general',		'General'),
	('Computational LinearEquation_computational matrixType symmetric',		'Symmetric'),
	('Computational LinearEquation_computational matrixType SPD',			'SPD'),	
	('Computational LinearEquation_computational matrixType Hermitian',		'Hermitian'),	
	('Computational LinearEquation_computational matrixType HPD',			'HPD'),	
	('Computational LinearEquation_computational matrixType triangular',		'Triangular'),	
	('Computational LinearEquation_computational storageType full',			'Full'),
	('Computational LinearEquation_computational storageType band',			'band'),
	('Computational LinearEquation_computational storageType packed',		'Packed'),
	('Computational LinearEquation_computational storageType tridiagonal',		'Tridiagonal'),
	('Computational LinearEquation_computational thePrecision single',		'Single'),
	('Computational LinearEquation_computational thePrecision double',		'Double'),
	


	('Driver LinearEquation_expert notes condition',				'estimate the matrix condition number'),
	('Driver LinearEquation_expert notes error',					'compute error bounds for the solution'),
	('Driver LinearEquation_expert notes refine',					'refine the solution'),
	('Driver LinearEquation_expert notes equilibrate',				'equilibrate the matrix'),
	('Driver LinearEquation_expert notes original',					'AX = B'),
	('Driver LinearEquation_expert notes transpose',				'A^TX = B'),
	('Driver LinearEquation_expert notes Hermitian_trans',				'A^HX = B'),
	('Driver LinearEquation_expert complex yes',					'Yes'),
	('Driver LinearEquation_expert complex no',					'No'),
	('Driver LinearEquation_expert matrixType general',				'General'),
	('Driver LinearEquation_expert matrixType symmetric',				'Symmetric'),
	('Driver LinearEquation_expert matrixType SPD',					'SPD'),	
	('Driver LinearEquation_expert matrixType Hermitian',				'Hermitian'),	
	('Driver LinearEquation_expert matrixType HPD',					'HPD'),	
	('Driver LinearEquation_expert storageType full',				'Full'),
	('Driver LinearEquation_expert storageType band',				'band'),
	('Driver LinearEquation_expert storageType packed',				'Packed'),
	('Driver LinearEquation_expert storageType tridiagonal',			'Tridiagonal'),
	('Driver LinearEquation_expert thePrecision single',				'Single'),
	('Driver LinearEquation_expert thePrecision double',				'Double'),


)
class AdvancedForm(forms.Form):
	advanced = forms.MultipleChoiceField(label='Please select the function(s) that you\'d like to execute.', required=True, widget=forms.CheckboxSelectMultiple(), choices=Advanced_choices)

	@staticmethod
	def find(answer):
		for item in Advanced_choices:
			if answer == item[0]:
				return item[1]



###--------------- Linear Equation Simple Driver Forms ------------------###
SimpleDriverComplex_choices = (
	('Driver LinearEquation_simple complex yes',					'Yes'),
	('Driver LinearEquation_simple complex no',					'No'),
)

class SimpleDriverComplexForm(forms.Form):
	SimpleDriverComplex = forms.ChoiceField(widget=forms.RadioSelect(), choices=SimpleDriverComplex_choices)



SimpleDriverMatrixType_choices = (
	('Driver LinearEquation_simple complex yes',					'Yes'),
	('Driver LinearEquation_simple complex no',					'No'),
	('Driver LinearEquation_simple matrixType general',				'General'),
	('Driver LinearEquation_simple matrixType symmetric',				'Symmetric'),
	('Driver LinearEquation_simple matrixType SPD',					'SPD'),	
	('Driver LinearEquation_simple matrixType Hermitian',				'Hermitian'),	
	('Driver LinearEquation_simple matrixType HPD',					'HPD'),	
)

class SimpleDriverMatrixTypeForm(forms.Form):
	SimpleDriverMatrixType = forms.ChoiceField(widget=forms.RadioSelect(), choices=SimpleDriverMatrixType_choices)



SimpleDriverStorageType_choices = (
	('Driver LinearEquation_simple storageType full',				'Full'),
	('Driver LinearEquation_simple storageType band',				'band'),
	('Driver LinearEquation_simple storageType packed',				'Packed'),
	('Driver LinearEquation_simple storageType tridiagonal',			'Tridiagonal'),
)

class SimpleDriverStorageTypeForm(forms.Form):
	SimpleDriverStorageType = forms.ChoiceField(widget=forms.RadioSelect(), choices=SimpleDriverStorageType_choices)



SimpleDriverPrecision_choices = (
	('Driver LinearEquation_simple thePrecision single',				'Single'),
	('Driver LinearEquation_simple thePrecision double',				'Double'),
)

class SimpleDriverPrecisionForm(forms.Form):
	SimpleDriverPrecision = forms.ChoiceField(widget=forms.RadioSelect(), choices=SimpleDriverPrecision_choices)



###--------------- Linear Equation Computational Forms ------------------###
ComputationalFunction_choices = (
	('Computational LinearEquation_computational notes factor',			'Factor a matrix (PA = LU)'),
	('Computational LinearEquation_computational notes error',			'Compute forward or backward error bounds for the solution to a linear system'),
	('Computational LinearEquation_computational notes refine',			'Refine the solution to a linear system'),
	('Computational LinearEquation_computational notes condition', 			'Estimate the condition number of a matrix'),
	('Computational LinearEquation_computational notes equilibrate',		'Equilibrate a matrix'),
	('Computational LinearEquation_computational notes inverse',			'Invert a matrix using provided factors (P, L, U)'),
	('Computational LinearEquation_computational notes solve',			'Solve a linear system using provided factors (P, L, U)'), 
)

class ComputationalFunctionForm(forms.Form):
	ComputationalFunction = forms.ChoiceField(widget=forms.RadioSelect(), choices=ComputationalFunction_choices)


ComputationalComplex_choices = (
	('Computational LinearEquation_computational complex yes',			'Yes'),
	('Computational LinearEquation_computational complex no',			'No'),
)

class ComputationalComplexForm(forms.Form):
	ComputationalComplex = forms.ChoiceField(widget=forms.RadioSelect(), choices=ComputationalComplex_choices)


ComputationalMatrixType_choices = (
	('Computational LinearEquation_computational matrixType general',		'General'),
	('Computational LinearEquation_computational matrixType symmetric',		'Symmetric'),
	('Computational LinearEquation_computational matrixType SPD',			'SPD'),	
	('Computational LinearEquation_computational matrixType Hermitian',		'Hermitian'),	
	('Computational LinearEquation_computational matrixType HPD',			'HPD'),	
	('Computational LinearEquation_computational matrixType triangular',		'Triangular'),	
)

class ComputationalMatrixTypeForm(forms.Form):
	ComputationalMatrixType = forms.ChoiceField(widget=forms.RadioSelect(), choices=ComputationalMatrixType_choices)


ComputationalStorageType_choices = (
	('Computational LinearEquation_computational storageType full',			'Full'),
	('Computational LinearEquation_computational storageType band',			'band'),
	('Computational LinearEquation_computational storageType packed',		'Packed'),
	('Computational LinearEquation_computational storageType tridiagonal',		'Tridiagonal'),
)

class ComputationalStorageTypeForm(forms.Form):
	ComputationalStorageType = forms.ChoiceField(widget=forms.RadioSelect(), choices=ComputationalStorageType_choices)


ComputationalPrecision_choices = (
	('Computational LinearEquation_computational thePrecision single',		'Single'),
	('Computational LinearEquation_computational thePrecision double',		'Double'),
)

class ComputationalPrecisionForm(forms.Form):
	ComputationalPrecision = forms.ChoiceField(widget=forms.RadioSelect(), choices=ComputationalPrecision_choices)




###--------------- Linear Equation Expert Driver Forms ------------------###
ExpertDriverEquation_choices = (
	('Driver LinearEquation_expert notes original',					'AX = B'),
	('Driver LinearEquation_expert notes transpose',				'A<sup>T</sup>X = B'),
	('Driver LinearEquation_expert notes Hermitian_trans',				'A<sup>H</sup>X = B'),
)

class ExpertDriverEquationForm(forms.Form):
	ExpertDriverEquation = forms.ChoiceField(label='', widget=forms.RadioSelect(), choices=ExpertDriverEquation_choices)


ExpertDriverFunction_choices = (
	('Driver LinearEquation_expert notes condition',				'estimate the matrix condition number'),
	('Driver LinearEquation_expert notes error',					'compute error bounds for the solution'),
	('Driver LinearEquation_expert notes refine',					'refine the solution'),
	('Driver LinearEquation_expert notes equilibrate',				'equilibrate the matrix'),
)

class ExpertDriverFunctionForm(forms.Form):
	ExpertDriverFunction = forms.ChoiceField(widget=forms.RadioSelect(), choices=ExpertDriverFunction_choices)


ExpertDriverComplex_choices = (
	('Driver LinearEquation_expert complex yes',					'Yes'),
	('Driver LinearEquation_expert complex no',					'No'),
)

class ExpertDriverComplexForm(forms.Form):
	ExpertDriverComplex = forms.ChoiceField(widget=forms.RadioSelect(), choices=ExpertDriverComplex_choices)


ExpertDriverMatrixType_choices = (
	('Driver LinearEquation_expert matrixType general',				'General'),
	('Driver LinearEquation_expert matrixType symmetric',				'Symmetric'),
	('Driver LinearEquation_expert matrixType SPD',					'SPD'),	
	('Driver LinearEquation_expert matrixType Hermitian',				'Hermitian'),	
	('Driver LinearEquation_expert matrixType HPD',					'HPD'),	
)

class ExpertDriverMatrixTypeForm(forms.Form):
	ExpertDriverMatrixType = forms.ChoiceField(widget=forms.RadioSelect(), choices=ExpertDriverMatrixType_choices)	


ExpertDriverStorageType_choices = (
	('Driver LinearEquation_expert storageType full',				'Full'),
	('Driver LinearEquation_expert storageType band',				'band'),
	('Driver LinearEquation_expert storageType packed',				'Packed'),
	('Driver LinearEquation_expert storageType tridiagonal',			'Tridiagonal'),
)

class ExpertDriverStorageTypeForm(forms.Form):
	ExpertDriverStorageType = forms.ChoiceField(widget=forms.RadioSelect(), choices=ExpertDriverStorageType_choices)	


ExpertDriverPrecision_choices = (
	('Driver LinearEquation_expert thePrecision single',				'Single'),
	('Driver LinearEquation_expert thePrecision double',				'Double'),
)

class ExpertDriverPrecisionForm(forms.Form):
	ExpertDriverPrecision = forms.ChoiceField(widget=forms.RadioSelect(), choices=ExpertDriverPrecision_choices)
