from django.conf import settings
from django.contrib.sites.models import RequestSite
from django.contrib.sites.models import Site

from registration import signals
from registration.forms import RegistrationForm
from registration.models import RegistrationProfile
from registration.backends.default import DefaultBackend

class EmailRegistrationForm(RegistrationForm):
    def __init__(self, *args, **kwargs):
        super(EmailRegistrationForm,self).__init__(*args, **kwargs)
        del self.fields['username']

    def clean(self):
        cleaned_data = super(EmailRegistrationForm,self).clean()
        if 'email' in self.cleaned_data:
            cleaned_data['username'] = self.cleaned_data['username'] = self.cleaned_data['email']
        return cleaned_data


class EmailBackend(DefaultBackend):
    def get_form_class(self, request):
        return EmailRegistrationForm

