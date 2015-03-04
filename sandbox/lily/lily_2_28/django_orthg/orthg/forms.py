from django import forms
from django.db.models import get_model
from orthg.models import *
from orthg.choiceDict import *


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
class standardGeneralizedForm(forms.Form):
    orthg_standardGeneralized = forms.ChoiceField(label='Which of the following least square problems would you like to compute?', widget=forms.RadioSelect(),choices=STANDARD_CHOICES)    
    

class FullStorageForm(forms.Form):
    orthg_FullStorage = forms.ChoiceField(label= 'Given your selections, the LAPACK subroutines only support full storage and general matrix. Do you wish to continue the search?',widget=forms.RadioSelect(),choices=NOYES_CHOICES)   
                        

##---- standard: Full Rank form ----##
class sFullRankForm(forms.Form):
    orthg_sFullRank = forms.ChoiceField(label='Does your matrix is full rank?',widget=forms.RadioSelect(),choices=NOYES_CHOICES)

     
##---- complex form ----##
class complexNumberForm(forms.Form):
    orthg_complexNumber = forms.ChoiceField(label='Does your matrix has complex number?', widget=forms.RadioSelect(), choices=NOYES_CHOICES)

##---- QR form ----##
class QRForm(forms.Form):
    orthg_QR = forms.ChoiceField(label='How would you like to compute problem?',widget=forms.RadioSelect(),choices=QR_CHOICES)
     
##---- SVD form ----##
class SVDForm(forms.Form):
    orthg_SVD = forms.ChoiceField(label='How would you like to compute problem?',widget=forms.RadioSelect(), choices=SVD_CHOICES)

##---- generalized:Full Rank form ----##

class gFullRankForm(forms.Form):
    orthg_gFullRank = forms.ChoiceField(label='How would you like to compute problem?',widget=forms.RadioSelect(),choices=GFULLRANK_CHOICES)
    
##--- precision form ---##
class singleDoubleForm(forms.Form):
    orthg_singleDouble = forms.ChoiceField(label='Would you like to use single or double precision?', widget=forms.RadioSelect(), choices=SINGLEDOUBLE_CHOICES)
