from django.conf.urls.defaults import *
from django.views.generic.simple import direct_to_template
from django.conf import settings

urlpatterns = patterns('lighthouse.views.lapack_eprob',
    url(r'^$', "lapack_eprob", name="lapack_eprob"),
	url(r'^clear/$', "eprob_clear"),
    #(r'^script/$', runScript),
)
