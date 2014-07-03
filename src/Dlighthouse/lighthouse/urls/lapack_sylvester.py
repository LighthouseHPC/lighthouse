from django.conf.urls.defaults import *
from django.views.generic.simple import direct_to_template
from django.conf import settings

urlpatterns = patterns('lighthouse.views.lapack_sylvester',
    url(r'^$', "index", name="lapack_sylvester"),
    url(r'^index/$', "index", name="lapack_sylvester"),
    url(r'^guidedSearch/$', "guidedSearch", name="lapack_sylvester_guidedSearch"),
    url(r'^advancedSearch/$', "advancedSearch", name="lapack_sylvester_advancedSearch"),
)
