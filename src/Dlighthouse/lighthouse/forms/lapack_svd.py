from django import forms
from django.db.models import get_model
from lighthouse.models.lapack_svd import *
from lighthouse.models.lapack_choiceDict import *
from lighthouse.forms.lapack_eigen import CustomRadioSelect
from django.core.exceptions import ValidationError
    

######-------- For Guided Search --------######
##---- problem form ---- ##
class problemForm(forms.Form):
    svd_problem = forms.ChoiceField(label='Which of the following singular value decomposition (SVD) problems do you have?',
					      widget=forms.RadioSelect(),
					      choices=SVD_CHOICES
					      )    


    
##---- complex form ----##
class complexNumberForm(forms.Form):
    svd_complexNumber = forms.ChoiceField(label='Does your matrix have any complex numbers?',
					      widget=forms.RadioSelect(),
					      choices=NOYES_CHOICES
					      )


##---- matrix type form ----##
class matrixTypeForm(forms.Form):
    svd_matrixType = forms.ChoiceField(label='What is the type of your matrix?', choices=[], widget=forms.RadioSelect())
    def __init__(self, request, *args, **kwargs):
	super(matrixTypeForm, self).__init__(*args, **kwargs)
	self.fields['svd_matrixType'].choices = request.session['Routines'].values_list('matrixType', 'matrixType').distinct()

	##--- order choices by string length ---##
	self.fields['svd_matrixType'].choices.sort(key=lambda k:len(k[1]))



##---- storage type form ----##
class storageTypeForm(forms.Form):
    svd_storageType = forms.ChoiceField(label='How is your matrix stored?', choices=[], widget=forms.RadioSelect())
    def __init__(self, request, *args, **kwargs):
	super(storageTypeForm, self).__init__(*args, **kwargs)
	self.fields['svd_storageType'].choices = request.session['Routines'].values_list('storageType', 'storageType').distinct()
	    


    
##--- svdvectors form ---##
class singularVectorsForm(forms.Form):
    svd_singularVectors = forms.ChoiceField(label='Do you need the singular vectors?', choices=NOYES_CHOICES, widget=forms.RadioSelect())
    def __init__(self, request, *args, **kwargs):
	super(singularVectorsForm, self).__init__(*args, **kwargs)
	self.fields['svd_singularVectors'].choices = request.session['Routines'].values_list('singularVectors', 'singularVectors').distinct()
	
	##--- if 'no/yes' is in teh choices, break it into 'no' and 'yes' ---##
	if (u'no/yes', u'no/yes') in self.fields['svd_singularVectors'].choices:
	    self.fields['svd_singularVectors'].choices = [(u'no', u'no'), (u'yes', u'yes')]
	    
	##--- if there is only one option and it is 'no', offer the option to stop the search ---##
	if self.fields['svd_singularVectors'].choices == [(u'no', u'no')]:
	    self.fields['svd_singularVectors'].label = 'Given your selections, the LAPACK subroutines do not provide singular vectors for your problem. Do you wish to continue the search?'
	    self.fields['svd_singularVectors'].choices = [(u'no', u'yes, continue'), (u'stop', u'no, stop the search')]
	    
	    
    

    
    
##--- precision form ---##
class singleDoubleForm(forms.Form):
    svd_singleDouble = forms.ChoiceField(label='Would you like to use single or double precision?',
                                              widget=forms.RadioSelect(),
                                              choices=SINGLEDOUBLE_CHOICES
                                              )
    
    
    
    
######-------- For advanced Search --------######
class advancedSearchMenuForm(forms.Form):
    advancedSearchMenu = forms.MultipleChoiceField(
	label = "Which of the following routine categories would you like to search?",
	widget=forms.CheckboxSelectMultiple(),
	choices=SVDMENU_CHOICES
	)
    

##--- for driver standard ---##    
class driver_standard_Form(forms.Form):
    driver_standard_driverComput = 'Driver'
    driver_standard_standardGeneralized = 'Standard'
    driver_standard_function = mark_safe('compute the SVD of an <i>m&timesn</i> matrix A')
    driver_standard_method = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=METHOD_CHOICES)
    driver_standard_singularVectors = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=NOYES_CHOICES)
    driver_standard_complexNumber = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=NOYES_CHOICES)
    driver_standard_singleDouble = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=SINGLEDOUBLE_CHOICES)
    

##--- for driver generalized ---##    
class driver_generalized_Form(forms.Form):
    driver_generalized_driverComput = 'Driver'
    driver_generalized_standardGeneralized = 'Generalized'
    driver_generalized_function = mark_safe('compute the GSVD of an <i>m&timesn</i> matrix A and a <i>p&timesn</i> matrix B')
    driver_generalized_method = 'QR algorithm'
    driver_generalized_singularVectors = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=NOYES_CHOICES)
    driver_generalized_complexNumber = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=NOYES_CHOICES)
    driver_generalized_singleDouble = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=SINGLEDOUBLE_CHOICES)
    

##--- for computational standard ---##    
class computational_standard_Form(forms.Form):
    computational_standard_driverComput = 'Computational'
    computational_standard_standardGeneralized = 'Standard'
    computational_standard_function = forms.MultipleChoiceField(
					    widget=forms.CheckboxSelectMultiple(attrs={'onchange':'showSVD("id_computational_standard_function_4", "showUL_computational_standard_function_4");'}),
					    choices=FUNCTION_STANDARD_CHOICES
					    )
    computational_standard_method = forms.MultipleChoiceField(
					    widget=forms.CheckboxSelectMultiple(attrs={'onclick':'SVDfunction(this, "id_computational_standard_function_4");'}),
					    choices=(
						(u'QR',				u'QR algorithm'),
						(u'divide-and-conquer',		u'divide and conquer'),
						(u'none',			u'none'),
						),
					    initial = (u'none',			u'none')
					    )
    computational_standard_singularVectors = forms.MultipleChoiceField(
					    widget=forms.CheckboxSelectMultiple(attrs={'onclick':'SVDfunction(this, "id_computational_standard_function_4");'}),
					    choices=NOYESNONE_CHOICES,
					    initial = NOYESNONE_CHOICES[2]
					    )
    computational_standard_complexNumber = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=NOYES_CHOICES)
    computational_standard_singleDouble = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=SINGLEDOUBLE_CHOICES)
	
	
	
   
##--- for computational generalized ---##
class computational_generalized_Form(forms.Form):
    computational_generalized_driverComput = 'Computational'
    computational_generalized_standardGeneralized = 'Generalized'
    computational_generalized_function = forms.MultipleChoiceField(
					    widget=forms.CheckboxSelectMultiple(attrs={'onchange':'showSVD("id_computational_generalized_function_1", "showUL_computational_generalized_function_1");'}),
					    choices=FUNCTION_GENERALIZED_CHOICES
					    )
    computational_generalized_method = 'QR algorithm'
    computational_generalized_singularVectors = forms.MultipleChoiceField(
					    widget=forms.CheckboxSelectMultiple(attrs={'onclick':'SVDfunction(this, "id_computational_generalized_function_1");'}),
					    choices=NOYESNONE_CHOICES,
					    initial = NOYESNONE_CHOICES[2]
					    )
    computational_generalized_complexNumber = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=NOYES_CHOICES)
    computational_generalized_singleDouble = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=SINGLEDOUBLE_CHOICES)