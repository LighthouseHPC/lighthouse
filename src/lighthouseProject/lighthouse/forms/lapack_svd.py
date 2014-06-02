from django import forms
from django.db.models import get_model
from lighthouse.models.lapack_svd import *
from lighthouse.models.lapack_choiceDict import *
from lighthouse.forms.lapack_eigen import CustomRadioSelect

#####------- Allow disabling options in a RadioSelect widget ----------#####
#from django.utils.safestring import mark_safe
#from django.utils.encoding import force_unicode
#
#class CustomRadioRenderer(forms.widgets.RadioFieldRenderer):
#    def render(self):
#        """ Disable some radio buttons based on disableList """
#	if self.disable == []:
#	    return mark_safe(u'<ul>\n%s\n</ul>' % u'\n'.join([u'<li>%s</li>' % force_unicode(w) for w in self]))
#	else:
#	    midList = []
#	    for x, wid in enumerate(self):
#		if self.disable[x] == True:
#		    wid.attrs['disabled'] = True
#		midList.append(wid)
#	    return mark_safe(u'<ul>\n%s\n</ul>' % u'\n'.join([u'<li>%s</li>' % w for w in midList]))
#
#
#class CustomRadioSelect(forms.widgets.RadioSelect):
#    renderer = CustomRadioRenderer
    
    
    
    
    
    

######-------- For Guided Search --------######
##---- problem form ---- ##
class problemForm(forms.Form):
    svd_problem = forms.ChoiceField(label='Which of the following problems about singular value decomposition (SVD) would you like to solve?',
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
    svd_singularVectors = forms.ChoiceField(label='Do you need the singular vectors?',
						widget=forms.RadioSelect(),
						choices=NOYES_CHOICES)
#    def __init__(self, request, *args, **kwargs):
#	super(singularVectorsForm, self).__init__(*args, **kwargs)
#	self.fields['svd_singularVectors'].choices = request.session['Routines'].values_list('singularVectors', 'singularVectors').distinct()
#	disableList = []
#	##--- if there is only one choice, show the others but disable them ---##
#	if len(self.fields['svd_singularVectors'].choices) == 1:
#	    selected = self.fields['svd_singularVectors'].choices[0][1]
#	    self.fields['svd_singularVectors'].choices = NOYES_CHOICES
#	    self.fields['svd_singularVectors'].initial = selected
#	    for item in self.fields['svd_singularVectors'].choices:
#		if item[1] != selected:
#		    disableList.append(True)
#		else:
#		    disableList.append(False)
#		    
#	self.fields['svd_singularVectors'].widget.renderer.disable = disableList
    
    
##--- condition number form ---##
#class cndNumberForm(forms.Form):
#    svd_cndNumber = forms.ChoiceField(label='Do you need a balancing transformation and/or a reciprocal condition number?',
#					      widget=forms.RadioSelect(),
#					      choices=NOYES_CHOICES
#					      )
    
    
    
##--- precision form ---##
class singleDoubleForm(forms.Form):
    svd_singleDouble = forms.ChoiceField(label='Would you like to use single or double precision?',
                                              widget=forms.RadioSelect(),
                                              choices=SINGLEDOUBLE_CHOICES
                                              )