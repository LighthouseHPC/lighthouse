from django.conf.urls.defaults import *
from django.views.generic.simple import direct_to_template
from django.conf import settings

urlpatterns = patterns('lighthouse.views.lapack_eigen',
    url(r'^$', "guidedSearch_problem", name="guidedSearch_problem"),
    url(r'^complex/$', "guidedSearch_complex", name="guidedSearch_complex"),
    url(r'^matrixType/$', "guidedSearch_matrixType", name="guidedSearch_matrixType"),
    #url(r'^$', "lapack_eigen", name="index"),
    #url(r'^clear_session/$', "eigen_clear_session"),  
    #url(r'^update_session/$', "eigen_update_session"),
)
