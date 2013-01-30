from django import forms
from haystack.forms import SearchForm


""" modify Haystack SearchForm """
CUSTOM_CHOICES = (
    ('blog.Post', 'Posts'),
    ('blog.Comment', 'Comments'),
)


class CustomSearchForm(SearchForm):
    def __init__(self, *args, **kwargs):
        super(CustomSearchForm, self).__init__(*args, **kwargs)
        self.fields['models'] = forms.MultipleChoiceField(
            label='Search In',
            choices=CUSTOM_CHOICES,
            required=False,
            widget=forms.CheckboxSelectMultiple)