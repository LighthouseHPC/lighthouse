from django import forms
from django.db.models import get_model
from lighthouse.models.lapack_svd import *
from lighthouse.models.lapack_choiceDict import *
from lighthouse.forms.lapack_eigen import CustomRadioSelect

    

######-------- For Guided Search --------######
##---- problem form ---- ##
class problemForm(forms.Form):
    svd_problem = forms.ChoiceField(label='Which of the following problems about singular value decomposition (SVD) do you have?',
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
	if len(self.fields['svd_matrixType'].choices) == 1:
		self.fields['svd_matrixType'].initial = self.fields['svd_matrixType'].choices[0][1]




##---- storage type form ----##
class storageTypeForm(forms.Form):
    svd_storageType = forms.ChoiceField(label='How is your matrix stored?', choices=[], widget=CustomRadioSelect())
    def __init__(self, request, *args, **kwargs):
	super(storageTypeForm, self).__init__(*args, **kwargs)
	self.fields['svd_storageType'].choices = request.session['Routines'].values_list('storageType', 'storageType').distinct()
	disableList = []
	
	##--- remove the choice full/packed/band/tridiagonal ---##
	if (u'bidiagonal/band', u'bidiagonal/band') in self.fields['svd_storageType'].choices:
	    self.fields['svd_storageType'].choices.remove((u'bidiagonal/band', u'bidiagonal/band'))
	    
	##--- if there is only one choice, show the others but disable them ---##
	if len(self.fields['svd_storageType'].choices) == 1:
	    selected = self.fields['svd_storageType'].choices[0][1]
	    self.fields['svd_storageType'].choices = (
		(u'full',                       u'full'),
		(u'band',                       u'band'),
		(u'packed',                     u'packed'),
		)
	    self.fields['svd_storageType'].initial = selected
	    for item in self.fields['svd_storageType'].choices:
		if item[1] != selected:
		    disableList.append(True)
		else:
		    disableList.append(False)
		    
	self.fields['svd_storageType'].widget.renderer.disable = disableList
	    


    
##--- svdvectors form ---##
class singularVectorsForm(forms.Form):
    svd_singularVectors = forms.ChoiceField(label='Do you need the singular vectors?', choices=NOYES_CHOICES, widget=CustomRadioSelect())
    def __init__(self, request, *args, **kwargs):
	super(singularVectorsForm, self).__init__(*args, **kwargs)
	self.fields['svd_singularVectors'].choices = request.session['Routines'].values_list('singularVectors', 'singularVectors').distinct()
	disableList = []
	##--- if there is only one choice, show the others but disable them ---##
	if len(self.fields['svd_singularVectors'].choices) == 1:
	    selected = self.fields['svd_singularVectors'].choices[0][1]
	    self.fields['svd_singularVectors'].choices = NOYES_CHOICES
	    self.fields['svd_singularVectors'].initial = selected
	    for item in self.fields['svd_singularVectors'].choices:
		if item[1] != selected:
		    disableList.append(True)
		else:
		    disableList.append(False)
	else:
	    self.fields['svd_singularVectors'].choices = NOYES_CHOICES
		    
	self.fields['svd_singularVectors'].widget.renderer.disable = disableList
    
    

    
    
##--- precision form ---##
class singleDoubleForm(forms.Form):
    svd_singleDouble = forms.ChoiceField(label='Would you like to use single or double precision?',
                                              widget=forms.RadioSelect(),
                                              choices=SINGLEDOUBLE_CHOICES
                                              )