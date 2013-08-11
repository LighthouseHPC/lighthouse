from django import forms

operation_choices = (
	('Solve a system of linear equation',	'Solve a system of linear equation'),
	('Compute eigenvalues',					'Compute eigenvalues'),
)

class petsc_problem_form(forms.Form):
	operations = forms.MultipleChoiceField( choices=operation_choices, label='Which of the following operations do you wish to perform?', widget=forms.RadioSelect(attrs={'onclick': "",'id':"main_op"}))


upload_choices = (
	('Yes',	'Yes (select if you want Lighthouse to compute the matrix properties)'),
	('No',	'No (select if you want to enter the matrix properties yourself)'),
)

solution_choices = (
	('Sequential',	'Sequential'),
	('Parallel', 	'Parallel'),
)

alt_choices = (
	('1','Download a PETSc program for computing matrix properties'),
	('2','Download a general PETSc program for solving a linear system'),
	('3','Upload the matrix property file created using our program'),
)

class solve_linear_system_form(forms.Form):
	upload_matrix      = forms.MultipleChoiceField( choices=upload_choices, label='Would you like to upload your matrix?', widget=forms.RadioSelect(attrs={'onclick': "upload_mat_changed();",'id':"upload_matrix"}))
	matrix_file        = forms.FileField( label='Select matrix file for upload.',widget=forms.ClearableFileInput(attrs={'onchange': "file_changed();",'id':"matrix_file"}))
	alt_choices        = forms.MultipleChoiceField( choices=alt_choices, label='Please select one of the following options.', widget=forms.RadioSelect(attrs={'onclick': "alt_choices_changed();",'id':"alt_choices"}))
	matrix_prop_file   = forms.FileField( label='Select matrix property file for upload.',widget=forms.ClearableFileInput(attrs={'onchange': "prop_file_changed();",'id':"matrix_prop_file"}))
	solution_type      = forms.MultipleChoiceField( choices=solution_choices, label='What type of solution do you need?', widget=forms.RadioSelect(attrs={'onclick': "solution_type_changed();",'id':"solution_type"}))

