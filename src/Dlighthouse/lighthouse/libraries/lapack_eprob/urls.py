from django.conf.urls.defaults import *
from django.views.generic.simple import direct_to_template
from django.conf import settings

urlpatterns = patterns('lighthouse.views.lapack_eprob',
    url(r'^$', "lapack_eprob", name="lapack_eprob"),
#	url(r'^$', direct_to_template, {'template': 'lighthouse/lapack_eprob/index2.html'}),
    url(r'^time/$',"current_datetime", name="current_datetime"),
	url(r'^time/plus/(\d{1,2})/$', "hours_ahead"),
	url(r'^guided/(\w+)/$', "eprob_guided"),
    #(r'^script/$', runScript),
)
