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
class standardGeneralizedForm(forms.ModelForm):
#      orthg_standardGeneralized = forms.ChoiceField(label = 'Which of the following least square problems would you like to compute?',widget = forms.RadioSelect())
      least_standardGeneralized = forms.ChoiceField(label = 'Which of the following least square problems would you like to compute?')
      class Meta:
            model = least 
            widget = forms.RadioSelect()
            fields = ('standardGeneralized',)
                   
class FullStorageForm(forms.ModelForm):
#      orthg_FullStorage = forms.ChoiceField(label = 'Given your selections, the LAPACK subroutines only support full storage and general matrix. Do you wish to continue the search?',widget = forms.RadioSelect())
      least_FullStorage = forms.ChoiceField(label = 'Given your selections, the LAPACK subroutines only support full storage and general matrix. Do you wish to continue the search?')
      class Meta:
            model = least
            widget = forms.RadioSelect()
            fields = ('FullStorage',)
            
##---- standard: Full Rank form ----##
class sFullRankForm(forms.ModelForm):
      least_sFullRank = forms.ChoiceField(label = 'Does your matrix is full rank?',widget = forms.RadioSelect())
      class Meta:
            model = least
            fields = ('sFullRank',)
                
##---- complex form ----##
class complexNumberForm(forms.ModelForm):
      least_complexNumber = forms.ChoiceField(label = 'Does your matrix has complex number?',widget = forms.RadioSelect())
      class Meta:
           model = least 
           fields = ('complexNumber',)
           
##---- QR form ----##
class QRForm(forms.ModelForm):
      least_qr = forms.ChoiceField(label = 'How would you like to compute problem?',widget = forms.RadioSelect())
      class Meta:
            model = least 
            fields = ('qr',)
           
##---- SVD form ----##
class SVDForm(forms.ModelForm):
      least_svd = forms.ChoiceField(label = 'How would you like to compute problem?',widget = forms.RadioSelect())
      class Meta:
            model = least 
            fields = ('svd',)
            
##---- generalized:Full Rank form ----##
class gFullRankForm(forms.ModelForm):
      least_gFullRank = forms.ChoiceField(label = 'How would you like to compute problem?',widget = forms.RadioSelect())
      class Meta:
            model = least 
            fields = ('gFullRank',)
           
##--- precision form ---##
class singleDoubleForm(forms.ModelForm):
      least_orthg_singleDouble = forms.ChoiceField(label = 'Would you like to use single or double precision?',widget = forms.RadioSelect())
      class Meta:
            model = least 
            fields = ('singleDouble',)
