from django.conf.urls.defaults import *
from django.views.generic.simple import direct_to_template
from django.conf import settings

urlpatterns = patterns('lighthouse.views.lapack_svd',
    url(r'^$', "index", name="lapack_svd"),
    url(r'^index/$', "index", name="lapack_svd"),
    url(r'^guidedSearch/$', "guidedSearch", name="lapack_svd_guidedSearch"),
    url(r'^advancedForm/$', "advancedForm", name="lapack_svd_advancedForm"),
    url(r'^advancedSearch/$', "advancedSearch", name="lapack_svd_advancedSearch"),
)
