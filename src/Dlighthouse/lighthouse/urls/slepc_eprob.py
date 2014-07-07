from django.conf.urls.defaults import *
from django.views.generic.simple import direct_to_template
from django.conf import settings

urlpatterns = patterns('lighthouse.views.slepc_eprob', # needs to be changed
    url(r'^$', "slepc_eprob", name="slepc_eprob"),
    url(r'^generateCode/$', "generateTemplate"),
    url(r'^update_session/$', "update_slepc_session"),
)
