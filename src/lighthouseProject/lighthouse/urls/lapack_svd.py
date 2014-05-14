from django.conf.urls.defaults import *
from django.views.generic.simple import direct_to_template
from django.conf import settings

urlpatterns = patterns('lighthouse.views.lapack_svd',
    url(r'^$', "guidedSearch_index", name="guidedSearch_index"),
    url(r'^problem/$', "guidedSearch_problem", name="guidedSearch_problem"),
    url(r'^standardGeneralized/$', "guidedSearch_standardGeneralized", name="guidedSearch_standardGeneralized"),
    url(r'^complexNumber/$', "guidedSearch_complexNumber", name="guidedSearch_complexNumber"),
    url(r'^matrixType/$', "guidedSearch_matrixType", name="guidedSearch_matrixType"),
    url(r'^storageType/$', "guidedSearch_storageType", name="guidedSearch_storageType"),
    url(r'^singularValues/$', "guidedSearch_singularValues", name="guidedSearch_singularValues"),
    url(r'^singularVectors/$', "guidedSearch_singularVectors", name="guidedSearch_singularVectors"),
    #url(r'^schur/$', "guidedSearch_schur", name="guidedSearch_schur"),
    #url(r'^cndNumber/$', "guidedSearch_cndNumber", name="guidedSearch_cndNumber"),
    url(r'^thePrecision/$', "guidedSearch_thePrecision", name="guidedSearch_thePrecision"),

    #url(r'^clear_session/$', "svd_clear_session"),  
    #url(r'^update_session/$', "svd_update_session"),
)
