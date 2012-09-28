from django.conf import settings
from django.contrib.sites.models import RequestSite
from django.contrib.sites.models import Site
from django.contrib.auth.models import User
from django import forms

from registration import signals
from registration.forms import RegistrationForm
from registration.models import RegistrationProfile
from registration.backends.default import DefaultBackend

from django.http import HttpResponseBadRequest

class EmailRegistrationForm(RegistrationForm):
    def __init__(self, *args, **kwargs):
        super(EmailRegistrationForm,self).__init__(*args, **kwargs)
        del self.fields['username']

    def clean(self):
        cleaned_data = super(EmailRegistrationForm,self).clean()
        if 'email' in self.cleaned_data:
            cleaned_data['username'] = self.cleaned_data['username'] = self.cleaned_data['email']
        else:
            """ When submit an empty form, show required fields without raising a new ValidationError.
            """
            return HttpResponseBadRequest()

        if User.objects.filter(email__iexact=self.cleaned_data['email']):
            raise forms.ValidationError("This email address is already in use. Please supply a different email address.")
        return cleaned_data


class EmailBackend(DefaultBackend):
    def get_form_class(self, request):
        return EmailRegistrationForm

