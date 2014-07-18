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
	choices=MENU_CHOICES
	)
    

def set_field_html_name(cls, new_name):
    """
    This creates wrapper around the normal widget rendering, 
    allowing for a custom field name (new_name).
    """
    old_render = cls.widget.render
    def _widget_render_wrapper(name, value, attrs=None):
        return old_render(new_name, value, attrs)

    cls.widget.render = _widget_render_wrapper
    
    
    
##--- for driver standard ---##    
class driver_standard_Form(forms.Form):
    driverComput = 'Driver'
    standardGeneralized = 'Standard'
    function = mark_safe('compute the SVD of an <i>m&timesn</i> matrix A')
    complexNumber = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(attrs={'id': 'id_driver_standard_complexNumber'}), choices=NOYES_CHOICES)
    method = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(attrs={'id': 'id_driver_standard_method'}), choices=METHOD_CHOICES)
    singularVectors = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(attrs={'id': 'id_driver_standard_singularVectors'}), choices=NOYES_CHOICES)
    singleDouble = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(attrs={'id': 'id_driver_standard_singleDouble'}), choices=SINGLEDOUBLE_CHOICES)
    
    for field in [complexNumber, method, singularVectors, singleDouble]:
	set_field_html_name(field, 'test')
	   
#    def clean_field1(self):
#	# The form field will be submit with the new name (instead of the name "field1").
#	data = self.data['driver_standard_method']
#	if data:
#	    raise ValidationError("Missing input")
#	return data




##--- for driver generalized ---##    
class driver_generalized_Form(forms.Form):
    driverComput = 'Driver'
    standardGeneralized = 'Generalized'
    function = mark_safe('compute the GSVD of an <i>m&timesn</i> matrix A and a <i>p&timesn</i> matrix B')
    complexNumber = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=NOYES_CHOICES)
    method = 'QR algorithm'
    singularVectors = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=NOYES_CHOICES)
    singleDouble = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=SINGLEDOUBLE_CHOICES)
    

##--- for computational standard ---##    
class computational_standard_Form(forms.Form):
    driverComput = 'Computational'
    standardGeneralized = 'Standard'
    function = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=FUNCTION_STANDARD_CHOICES)
    complexNumber = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=NOYES_CHOICES)
    method = forms.MultipleChoiceField(
					    widget=forms.CheckboxSelectMultiple(),
					    choices=(
						(u'QR',				u'QR algorithm'),
						(u'divide-and-conquer',		u'divide and conquer'),
						)
					    )
    singularVectors = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=NOYES_CHOICES)
    singleDouble = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=SINGLEDOUBLE_CHOICES)
   
   
##--- for computational generalized ---##
class computational_generalized_Form(forms.Form):
    driverComput = 'Computational'
    standardGeneralized = 'Generalized'
    function = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=FUNCTION_GENERALIZED_CHOICES)
    complexNumber = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=NOYES_CHOICES)
    method = 'QR algorithm'
    singularVectors = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=NOYES_CHOICES)
    singleDouble = forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple(), choices=SINGLEDOUBLE_CHOICES)    