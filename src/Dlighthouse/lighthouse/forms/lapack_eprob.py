from django import forms
from django.db.models import get_model
from lighthouse.models.lapack_eprob import *

#
#
# Helper function - performs a lookup against eprob_fields and builds a field from that information
#
#

def makeFieldRadio(name):
    if name in eprob_fields:
    	field_label, field_choices = eprob_fields[name]
    	return forms.ChoiceField(label=field_label, choices=field_choices, widget=forms.RadioSelect())
    else:
        return forms.ChoiceField(widget=forms.RadioSelect)

def makeFieldCheckbox(name):
    if name in eprob_fields:        
    	field_label, field_choices = eprob_fields[name]
    	return forms.MultipleChoiceField(label=field_label, choices=field_choices, widget=forms.CheckboxSelectMultiple()
, required = False)
    else:
        return forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple)

#
#
# Form Classes
#
#

# Basic guided form - uses all the choices available
class GuidedForm(forms.Form):
    def __init__(self, name = 'problem' , *args, **kwargs):
        super(GuidedForm, self).__init__(*args, **kwargs)
        self.fields[name] = makeFieldRadio(name)

# Filtered guided form - always a subset of GuidedForm() and only contains relevant choices
class FilteredForm(forms.Form):
    def __init__(self, name, filtered, *args, **kwargs):
        super(FilteredForm, self).__init__(*args, **kwargs)
        self.fields[name] = forms.ChoiceField(label=eprob_fields[name][0],
                choices=getFilteredChoices(filtered,name), widget=forms.RadioSelect())

# Advanced form - builds a form with multiple fields using the page definitions
class AdvancedForm(forms.Form):
	def __init__(self, formname, *args, **kwargs):
		super(AdvancedForm, self).__init__(*args, **kwargs)
		for label in eprob_advanced_forms[formname]:
			self.fields[label] = makeFieldCheckbox(label)

# Advanced filtered form - subset of AdvancedForm(), only contains fields/forms relevant
class AdvancedFilteredForm(forms.Form):
    def __init__(self, name,filtered, *args, **kwargs):
        super(AdvancedFilteredForm, self).__init__(*args, **kwargs)
        formslist = getFilteredChoicesAdvanced(filtered, name)
        for formname,field_label,field_choices in formslist:
            self.fields[formname] = forms.MultipleChoiceField(label=field_label, choices=field_choices, widget=forms.CheckboxSelectMultiple(), required = False)