from django import forms
from django.db.models import get_model
from lighthouse.models.lapack_eigen import *



###--- Guidede Search Forms: performs a lookup against eigen_fields and builds a field from that information ---###

class GuidedForm(forms.Form):
    def __init__(self, name , *args, **kwargs):
        super(GuidedForm, self).__init__(*args, **kwargs)
        if name in eigen_fields:
           self.fields[name] = forms.ChoiceField(label = eigen_fields[name][0], choices= eigen_fields[name][1], widget=forms.RadioSelect())
	else:
	    self.fields[name] = forms.ChoiceField(widget=forms.RadioSelect)


# Filtered guided form - always a subset of GuidedForm() and only contains relevant choices
class FilteredForm(forms.Form):
    def __init__(self, name, filtered, *args, **kwargs):
        super(FilteredForm, self).__init__(*args, **kwargs)
        self.fields[name] = forms.ChoiceField(label=eigen_fields[name][0],
                choices=getFilteredChoices(filtered,name), widget=forms.RadioSelect())




###--- Advanced Search Forms ---###

def makeFieldCheckbox(name):
    if name in eigen_fields:        
    	field_label, field_choices = eigen_fields[name]
    	return forms.MultipleChoiceField(label=field_label, choices=field_choices, widget=forms.CheckboxSelectMultiple(), required = False)
    else:
        return forms.MultipleChoiceField(widget=forms.CheckboxSelectMultiple)


# Advanced form - builds a form with multiple fields using the page definitions
class AdvancedForm(forms.Form):
	def __init__(self, formname, *args, **kwargs):
		super(AdvancedForm, self).__init__(*args, **kwargs)
		for label in eigen_advanced_forms[formname]:
			self.fields[label] = makeFieldCheckbox(label)

# Advanced filtered form - subset of AdvancedForm(), only contains fields/forms relevant
class AdvancedFilteredForm(forms.Form):
    def __init__(self, name,filtered, *args, **kwargs):
        super(AdvancedFilteredForm, self).__init__(*args, **kwargs)
        formslist = getFilteredChoicesAdvanced(filtered, name)
        for formname,field_label,field_choices in formslist:
            self.fields[formname] = forms.MultipleChoiceField(label=field_label, choices=field_choices, widget=forms.CheckboxSelectMultiple(), required = False)
