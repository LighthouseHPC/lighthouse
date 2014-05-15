from django import forms
from django.db.models import get_model
from lighthouse.models.lapack_svd import *
from lighthouse.models.lapack_choiceDict import *

#####------- Allow disabling options in a RadioSelect widget ----------#####
from django.utils.safestring import mark_safe
from django.utils.encoding import force_unicode

class CustomRadioRenderer(forms.widgets.RadioFieldRenderer):
    def render(self):
        """ Disable some radio buttons based on disableList """
	if self.disable == []:
	    return mark_safe(u'<ul>\n%s\n</ul>' % u'\n'.join([u'<li>%s</li>' % force_unicode(w) for w in self]))
	else:
	    midList = []
	    for x, wid in enumerate(self):
		if self.disable[x] == True:
		    wid.attrs['disabled'] = True
		midList.append(wid)
	    return mark_safe(u'<ul>\n%s\n</ul>' % u'\n'.join([u'<li>%s</li>' % w for w in midList]))


class CustomRadioSelect(forms.widgets.RadioSelect):
    renderer = CustomRadioRenderer
    
    
    
    
    
    

######-------- For Guided Search --------######
##---- problem form ---- ##
class problemForm(forms.Form):
    svd_prob = forms.ChoiceField(label='Which of the following problems would you like to compute?',
					      widget=forms.RadioSelect(),
					      choices=SVD_CHOICES
					      )    


##---- standard/generalized form ---##
class standardGeneralizedForm(forms.Form):
    svd_standardGeneralized = forms.ChoiceField(label='Is the problem standard or generalized?',
					      widget=forms.RadioSelect(),
					      choices=STANDARD_CHOICES,
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
	    

	    

##--- selected svdvalue form ---##
class selectedEVForm(forms.Form):
    svd_selectedEV = forms.ChoiceField(label='Do you only need svdvalues within a specific range?',
					 widget=forms.RadioSelect(),
					 choices=NOYES_CHOICES)


    
##--- svdvectors form ---##
class svdvectorForm(forms.Form):
    svd_svdvector = forms.ChoiceField(label='Do you need svdvectors?',
					      widget=forms.RadioSelect(),
					      choices=NOYES_CHOICES
					      )
    
    
     
     
##--- condition numbers for svdvectors form ---##
class cndN_svdvectorForm(forms.Form):
    svd_cndN_svdvector = forms.ChoiceField(label='Would you like to compute the reciprocal condition numbers for the svdvectors?',
					      widget=forms.RadioSelect(),
					      choices=NOYES_CHOICES
					      )
    
    
         
##--- svdvectors or Schur form ---##
class schurForm(forms.Form):
    svd_schur = forms.ChoiceField(label='In addition to svdvalues, do you need other properties such as Schur form, Schur vectors, and sorted svdvalues?',
					      widget=forms.RadioSelect(),
					      choices=NOYES_CHOICES
					      )
    
    
##--- condition number form ---##
class cndNumberForm(forms.Form):
    svd_cndNumber = forms.ChoiceField(label='Do you need a balancing transformation and/or a reciprocal condition number?',
					      widget=forms.RadioSelect(),
					      choices=NOYES_CHOICES
					      )
    
    
    
##--- precision form ---##
class thePrecisionForm(forms.Form):
    svd_thePrecision = forms.ChoiceField(label='Would you like to use single or double precision?',
					      widget=forms.RadioSelect(),
					      choices=SINGLEDOUBLE_CHOICES
					      )