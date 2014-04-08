from django import forms
from django.db.models import get_model
from lighthouse.models.lapack_eigen import *



###-------- For Guided Search --------###

Problem_choices = (
	('standard',			u'Standard eigenproblem'),
	('generalized', 		u'Generalized eigenproblem'),
	('sylvester',			u'Sylvester matrix equation'),
	('svd',				u'Singular value decomposition'),
)

class problemForm(forms.Form):
    eigen_prob = forms.ChoiceField(label='Which of the following problems would you like to solve?',
					      widget=forms.RadioSelect(),
					      choices=Problem_choices
					      )    



complex_choices = (
    ('real',	'No'),
    ('complex',	'Yes'),
    )

class complexForm(forms.Form):
    eigen_complex = forms.ChoiceField(label='Does your matrix have any complex numbers?',
					      widget=forms.RadioSelect(),
					      choices=complex_choices
					      )



matrixType_real_choices = (
    ('symmetric',	'Symmetric'),
    ('rGeneral',	'General'),
    ('spd',		'Symmetric positive definite (SPD)'),
    ('rHessenberg',	'Upper Hessenberg')
)

matrixType_complex_choices = (
    ('hermitian',	'Hermitian'),
    ('cGeneral',	'General'),
    ('hpd',		'Hermitian positive definite (HPD)'),
    ('cHessenberg',	'Upper Hessenberg')
)

class matrixTypeForm(forms.Form):
        eigen_matrixType = forms.ChoiceField(label='What is the type of your matrix?', choices=[], widget=forms.RadioSelect())
        def __init__(self, request, *args, **kwargs):
                super(matrixTypeForm, self).__init__(*args, **kwargs)
		#print request.session['eigen_complex']
		if request.session['eigen_complex'] == 'real':
		    self.fields['eigen_matrixType'].choices = matrixType_real_choices
		else:
		    self.fields['eigen_matrixType'].choices = matrixType_complex_choices
                ##--- order choices by string length ---##
                self.fields['eigen_matrixType'].choices.sort(key=lambda k:len(k[1]))
                if len(self.fields['eigen_matrixType'].choices) == 1:
                        self.fields['eigen_matrixType'].initial = self.fields['eigen_matrixType'].choices[0][1]

