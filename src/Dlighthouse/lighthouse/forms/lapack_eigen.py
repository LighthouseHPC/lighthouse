from django import forms
from django.db.models import get_model
from lighthouse.models.lapack_eigen import *
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
    eigen_problem = forms.ChoiceField(label='Which of the following problems would you like to compute?',
					      widget=forms.RadioSelect(),
					      choices=EIGENPROBLEM_CHOICES
					      )    


##---- standard/generalized form ---##
class standardGeneralizedForm(forms.Form):
    eigen_standardGeneralized = forms.ChoiceField(label='Is the problem standard or generalized?',
					      widget=forms.RadioSelect(),
					      choices=STANDARD_CHOICES,
					)
    
    
##---- complex form ----##
class complexNumberForm(forms.Form):
    eigen_complexNumber = forms.ChoiceField(label='Does your matrix have any complex numbers?',
					      widget=forms.RadioSelect(),
					      choices=NOYES_CHOICES
					      )


##---- matrix type form ----##
class matrixTypeForm(forms.Form):
    eigen_matrixType = forms.ChoiceField(label='What is the type of your matrix?', choices=[], widget=forms.RadioSelect())
    def __init__(self, request, *args, **kwargs):
	super(matrixTypeForm, self).__init__(*args, **kwargs)
	self.fields['eigen_matrixType'].choices = request.session['Routines'].values_list('matrixType', 'matrixType').distinct()
	##--- display full names for semidefinite, SPD and HPD ---##
	for i, item in enumerate(self.fields['eigen_matrixType'].choices):
		if 'SPD' in item:
			self.fields['eigen_matrixType'].choices[i] = (u'SPD', u'real symmetric positive definite (SPD)')
		elif 'HPD' in item:
			self.fields['eigen_matrixType'].choices[i] = (u'HPD', u'complex Hermitian positive definite (HPD)')
	##--- order choices by string length ---##
	self.fields['eigen_matrixType'].choices.sort(key=lambda k:len(k[1]))
	if len(self.fields['eigen_matrixType'].choices) == 1:
		self.fields['eigen_matrixType'].initial = self.fields['eigen_matrixType'].choices[0][1]




##---- storage type form ----##
class storageTypeForm(forms.Form):
    eigen_storageType = forms.ChoiceField(label='How is your matrix stored?', choices=[], widget=CustomRadioSelect())
    def __init__(self, request, *args, **kwargs):
	super(storageTypeForm, self).__init__(*args, **kwargs)
	self.fields['eigen_storageType'].choices = request.session['Routines'].values_list('storageType', 'storageType').distinct()
	disableList = []
	
	##--- handle the choice full/packed/band/tridiagonal ---##
	if (u'full/packed/band/tridiagonal', u'full/packed/band/tridiagonal') in self.fields['eigen_storageType'].choices:
	    for item in 'full/packed/band/tridiagonal'.split('/'):
		self.fields['eigen_storageType'].choices = self.fields['eigen_storageType'].choices+ [(item.decode('unicode-escape'), item.decode('unicode-escape')),]
	    self.fields['eigen_storageType'].choices.remove((u'full/packed/band/tridiagonal', u'full/packed/band/tridiagonal'))
	    self.fields['eigen_storageType'].choices = list(set(self.fields['eigen_storageType'].choices))
	    
	##--- order choices by string length ---##
	self.fields['eigen_storageType'].choices.sort(key=lambda k:len(k[1]))
	    
	##--- if there is only one choice, show the others but disable them ---##
	if len(self.fields['eigen_storageType'].choices) == 1:
	    selected = self.fields['eigen_storageType'].choices[0][1]
	    self.fields['eigen_storageType'].choices = (
		(u'full',                       u'full'),
		(u'band',                       u'band'),
		(u'packed',                     u'packed'),
		)
	    self.fields['eigen_storageType'].initial = selected
	    for item in self.fields['eigen_storageType'].choices:
		if item[1] != selected:
		    disableList.append(True)
		else:
		    disableList.append(False)
		    
	self.fields['eigen_storageType'].widget.renderer.disable = disableList
	    

	    

##--- selected eigenvalue form ---##
class selectedEVForm(forms.Form):
    eigen_selectedEV = forms.ChoiceField(label='Do you only need eigenvalues within a specific range?',
					 widget=forms.RadioSelect(),
					 choices=NOYES_CHOICES)


    
##--- eigenvectors form ---##
class eigenvectorForm(forms.Form):
    eigen_eigenvector = forms.ChoiceField(label='Do you need eigenvectors?',
					      widget=forms.RadioSelect(),
					      choices=NOYES_CHOICES
					      )
    
    
         
##--- eigenvectors or Schur form ---##
class schurForm(forms.Form):
    eigen_schur = forms.ChoiceField(label='In addition to eigenvalues, do you need other properties such as Schur form, Schur vectors, and sorted eigenvalues?',
					      widget=forms.RadioSelect(),
					      choices=NOYES_CHOICES
					      )
    
    
##--- condition number form ---##
class cndNumberForm(forms.Form):
    eigen_cndNumber = forms.ChoiceField(label='Do you need a balancing transformation and/or a reciprocal condition number?',
					      widget=forms.RadioSelect(),
					      choices=NOYES_CHOICES
					      )
    
    
    
##--- precision form ---##
class singleDoubleForm(forms.Form):
    eigen_singleDouble = forms.ChoiceField(label='Would you like to use single or double precision?',
					      widget=forms.RadioSelect(),
					      choices=SINGLEDOUBLE_CHOICES
					      )