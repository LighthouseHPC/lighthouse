from django import forms
from django.db.models import get_model





### --- for simple search --- ###

Problem_choices = (
	('Driver LinearEquation_simple solve',					'Solve a linear equation only'),
	('Computational LinearEquation_computational factor',			'Factor a matrix (PA = LU)'),
	('Computational LinearEquation_computational refine',			'Refine the solution to a linear system'),
	('Computational LinearEquation_computational error',			'Compute forward or backward error bounds for the solution to a linear system'),
	('Computational LinearEquation_computational condition', 		'Estimate the condition number of a matrix'),
	('Computational LinearEquation_computational equilibrate',		'Equilibrate a matrix'),
	('Computational LinearEquation_computational inverse',			'Invert a matrix using provided factors (P, L, U)'),
	('Driver LinearEquation_expert',					'Solve a linear equation PLUS'),
	('Driver LinearEquation_expert refine',					'Refine the solution'),
	('Driver LinearEquation_expert error',					'Compute forward or backward error bounds for the solution'),
	('Driver LinearEquation_expert condition', 				'Estimate the condition number of a matrix'),
	('Driver LinearEquation_expert equilibrate',				'Equilibrate a matrix'),
)



class ProblemForm(forms.Form):
	question_prob = forms.MultipleChoiceField(label='Which of the following functions do you wish to execute?', required=True, widget=forms.CheckboxSelectMultiple(), choices=Problem_choices)

	@staticmethod
	def find(answer):
		for item in Problem_choices:
			if answer == item[0]:
				return item[1]




Equation_choices = (
	('original',				'AX = B'),
	('transpose',				'A<sup>T</sup>X = B'),
	('Hermitian_trans',			'A**H*X = B'),
)


class EquationForm(forms.Form):
	question_equa = forms.ChoiceField(label='What form of the linear system do you want to solve?', widget=forms.RadioSelect(), choices=Equation_choices)




class FactorForm(forms.Form):
	question_fact = forms.ChoiceField(label='Is your matrix factorized?', choices=([('y','Yes'), ('n','No')]), widget=forms.RadioSelect(), initial = dict())





class ComplexForm(forms.Form):
	question_comp = forms.ChoiceField(label='Are there complex numbers in your matrix?', choices=([('y','Yes'), ('n','No'),('what', 'I don\'t know'),]), widget=forms.RadioSelect(), initial = dict())





class MatrixTypeForm(forms.Form):
	question_type = forms.ChoiceField(label='What is the type of your matrix?', choices=[], widget=forms.RadioSelect())
	def __init__(self, request, *args, **kwargs):
		super(MatrixTypeForm, self).__init__(*args, **kwargs)
		self.fields['question_type'].choices = request.session['Routines'].values_list('matrixType', 'matrixType').distinct()





class StorageForm(forms.Form):
	question_stor = forms.ChoiceField(label='How is your matrix stored?', choices=[], widget=forms.RadioSelect())
	def __init__(self, request, *args, **kwargs):
		super(StorageForm, self).__init__(*args, **kwargs)
		self.fields['question_stor'].choices = request.session['Routines'].values_list('storageType', 'storageType').distinct()




class PrecisionForm(forms.Form):
	question_prec = forms.ChoiceField(label='Would you like to use single or double precision?', choices=([('s','Single'), ('d','Double'), ('what', 'I don\'t know'),]), widget=forms.RadioSelect())



### --- for advanced search --- ###
'''
AdvancedProblem_choices = (
	('original',				'A*X = B'),
	('factor',				'Factor a matrix (PA = LU)'),
	('error bounds',			'Compute forward or backward error bounds for the solution to a linear system and refine the solution'),
	('condition number', 			'Estimate the condition number of a matrix'),
	('equilibrate',				'Equilibrate a matrix'),
	('inverse',				'Invert a matrix using provided factors (P, L, U)'),
	('transpose',				'A**T*X = B'),
	('Hermitian_trans',			'A**H*X = B'),
	('expert',				'Solve a linear equation PLUS'),
	('expert refine',			'Refine the solution'),
	('expert error bounds',			'Compute forward or backward error bounds for the solution'),
	('expert condition number', 		'Estimate the condition number of a matrix'),
	('expert equilibrate',			'Equilibrate a matrix'),
)
'''	


Advanced_choices = (
	('simple complex yes',				'Yes'),
	('simple complex no',				'No'),
	('simple matrix general',			'General'),
	('simple matrix symmetric',			'Symmetric'),
	('simple matrix SPD',				'SPD'),	
	('simple matrix Hermitian',			'Hermitian'),	
	('simple matrix HPD',				'HPD'),	
	('simple storage full',				'Full'),
	('simple storage band',				'band'),
	('simple storage packed',			'Packed'),
	('simple storage tridiagonal',			'Tridiagonal'),
	('simple precision single',			'Single'),
	('simple precision double',			'Double'),
	

	('computational factor',			'Factor a matrix (PA = LU)'),
	('computational error bounds',			'Compute forward or backward error bounds for the solution to a linear system'),
	('computational refine',			'Refine the solution to a linear system'),
	('computational condition number', 		'Estimate the condition number of a matrix'),
	('computational equilibrate',			'Equilibrate a matrix'),
	('computational inverse',			'Invert a matrix using provided factors (P, L, U)'),
	('computational solve',				'Solve a linear system using provided factors (P, L, U)'), 
	('computational complex yes',			'Yes'),
	('computational complex no',			'No'),
	('computational matrix general',		'General'),
	('computational matrix symmetric',		'Symmetric'),
	('computational matrix SPD',			'SPD'),	
	('computational matrix Hermitian',		'Hermitian'),	
	('computational matrix HPD',			'HPD'),	
	('computational matrix triangular',		'Triangular'),	
	('computational storage full',			'Full'),
	('computational storage band',			'band'),
	('computational storage packed',		'Packed'),
	('computational storage tridiagonal',		'Tridiagonal'),
	('computational precision single',		'Single'),
	('computational precision double',		'Double'),
	


	('expert notes condition number',		'estimate the matrix condition number'),
	('expert notes error bound',			'compute error bounds for the solution'),
	('expert notes refine',				'refine the solution'),
	('expert notes equilibrate',			'equilibrate the matrix'),
	('expert notes original',			'AX = B'),
	('expert notes transpose',			'A^TX = B'),
	('expert notes Hermitian_trans',		'A^HX = B'),
	('expert complex yes',				'Yes'),
	('expert complex no',				'No'),
	('expert matrix general',			'General'),
	('expert matrix symmetric',			'Symmetric'),
	('expert matrix SPD',				'SPD'),	
	('expert matrix Hermitian',			'Hermitian'),	
	('expert matrix HPD',				'HPD'),	
	('expert storage full',				'Full'),
	('expert storage band',				'band'),
	('expert storage packed',			'Packed'),
	('expert storage tridiagonal',			'Tridiagonal'),
	('expert precision single',			'Single'),
	('expert precision double',			'Double'),


)
class AdvancedForm(forms.Form):
	advanced = forms.MultipleChoiceField(label='Please select the function(s) that you\'d like to execute.', required=True, widget=forms.CheckboxSelectMultiple(), choices=Advanced_choices)

#	@staticmethod
#	def find(answer):
#		for item in Advanced_choices:
#			if answer == item[0]:
#				return item[1]


