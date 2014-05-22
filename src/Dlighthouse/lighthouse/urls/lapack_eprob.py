from django.conf.urls.defaults import *
from django.views.generic.simple import direct_to_template
from django.conf import settings

urlpatterns = patterns('lighthouse.views.lapack_eprob',
    url(r'^$', "lapack_eprob", name="lapack_eprob"),
    url(r'^clear_session/$', "eprob_clear_session"),  
    url(r'^update_session/$', "eprob_update_session"),
    #(r'^script/$', runScript),
)
