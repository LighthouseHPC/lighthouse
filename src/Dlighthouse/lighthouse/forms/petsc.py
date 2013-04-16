from django import forms

operation_choices = (
	('Solve a system of linear equation',	'Solve a system of linear equation'),
	('Compute eigenvalues',					'Compute eigenvalues'),
)

class petsc_problem_form(forms.Form):
	operations = forms.MultipleChoiceField( choices=operation_choices, label='Which of the following operations do you wish to perform?', widget=forms.RadioSelect(attrs={'onclick': "",'id':"main_op"}))


upload_choices = (
	('Yes',	'Yes (select if you want Lighthouse to compute the matrix properties.)'),
	('No',	'No (select if you want to enter the matrix properties yourself.)'),
)

solution_choices = (
	('Sequential',	'Sequential'),
	('Parallel', 	'Parallel'),
)

output_choices = (
	('PETSc binary', 'PETSc binary'),
	('MATLAB',		 'MATLAB'),
)

output_choices = (
	('PETSc binary format', 'PETSc binary format'),
	('MATLAB format',		 'MATLAB format'),
)

intput_choices = (
	('PETSc binary format',				'PETSc binary format'),
	('Harwell-Boeing Exchange format',	'Harwell-Boeing Exchange format'),	
	('Matrix Market Exchange format',	'Matrix Market Exchange format'),
	('MATLAB format',					'MATLAB format'),
)

complex_number_choices = (
	('Yes',	'Yes'),
	('No',	'No'),
)

nonzero_structure_choices = (
	('Diagonal',			'Diagonal'),
	('Tridiagonal',			'Tridiagonal'),
	('Banded',	    		'Banded'),
	('Sparse',	    		'Sparse'),
	('Sparse symmteric',	'Sparse symmteric'),
	('Sparse unsymmteric',	'Sparse unsymmteric'),
	('Triangular',			'Triangular'),
)

numerical_symmetry_choices = (
	('Symmteric',							'Symmteric'),
	('Symmteric positive definite',			'Symmteric positive definite'),
	('Symmteric positive semi-definite',	'Symmteric positive semi-definite'),
	('Symmetric indefinite',	    		'Symmetric indefinite'),
	('Unsymmetric',							'Unsymmetric'),
	('Hermitian',							'Hermitian'),
	('Skew symmetric',						'Skew symmetric'),
)

class solve_linear_system_form(forms.Form):
	upload_matrix      = forms.MultipleChoiceField( choices=upload_choices, label='Would you like to upload your matrix?', widget=forms.RadioSelect(attrs={'onclick': "upload_mat_changed();",'id':"upload_matrix"}))
	matrix_file        = forms.FileField( label='Select matrix file for upload.',widget=forms.ClearableFileInput(attrs={'onchange': "file_changed();",'id':"matrix_file"}))
	input_format       = forms.MultipleChoiceField( choices=intput_choices, label='What is the storage format of your matrix?', widget=forms.RadioSelect(attrs={'onclick': "input_format_changed();",'id':"input_format"}))
	row_count          = forms.CharField( label='Enter the number of rows in your matrix.', widget=forms.TextInput(attrs={'onkeyup': "row_count_changed();", 'onchange': "row_count_changed();", 'id':"row_count"}))
	column_count       = forms.CharField( label='Enter the number of columns in your matrix.', widget=forms.TextInput(attrs={'onkeyup': "column_count_changed();",'onchange': "column_count_changed();",'id':"column_count"}))
	complex_number     = forms.MultipleChoiceField( choices=complex_number_choices, label='Does your matrix contain complex numbers?', widget=forms.RadioSelect(attrs={'onclick': "complex_number_changed();",'id':"complex_number"}))
	nonzero_structure  = forms.MultipleChoiceField( choices=nonzero_structure_choices, label='What is the nonzero structure of your matrix?', widget=forms.RadioSelect(attrs={'onclick': "nonzero_structure_changed();",'id':"nonzero_structure"}))
	numerical_symmetry = forms.MultipleChoiceField( choices=numerical_symmetry_choices, label='What is the numercial symmetry property of your matrix?', widget=forms.RadioSelect(attrs={'onclick': "numerical_symmetry_changed();",'id':"numerical_symmetry"}))
	solution_type      = forms.MultipleChoiceField( choices=solution_choices, label='What type of solution do you need?', widget=forms.RadioSelect(attrs={'onclick': "solution_type_changed();",'id':"solution_type"}))
	output_format      = forms.MultipleChoiceField( choices=output_choices, label='Select an output format.', widget=forms.RadioSelect(attrs={'onclick': "",'id':"output_format"}))