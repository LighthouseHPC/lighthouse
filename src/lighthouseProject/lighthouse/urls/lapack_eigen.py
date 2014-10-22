from django.conf.urls.defaults import *
from django.views.generic.simple import direct_to_template
from django.conf import settings

urlpatterns = patterns('lighthouse.views.lapack_eigen',
    url(r'^$', "index", name="lapack_eigen"),
    url(r'^index/$', "index", name="lapack_eigen"),
    url(r'^guidedSearch/$', "guidedSearch", name="lapack_eigen_guidedSearch"),
    url(r'^advancedForm/$', "advancedForm", name="lapack_eigen_advancedForm"),
    url(r'^advancedSearch/$', "advancedSearch", name="lapack_eigen_advancedSearch"),
)
