from django.conf.urls.defaults import *
from django.views.generic.simple import direct_to_template
from django.conf import settings

urlpatterns = patterns('lighthouse.views.lapack_eigen',
    url(r'^$', "guidedSearch_index", name="guidedSearch_index"),
    url(r'^problem/$', "guidedSearch_problem", name="guidedSearch_problem"),
    url(r'^standardGeneralized/$', "guidedSearch_standardGeneralized", name="guidedSearch_standardGeneralized"),
    url(r'^complexNumber/$', "guidedSearch_complexNumber", name="guidedSearch_complexNumber"),
    url(r'^matrixType/$', "guidedSearch_matrixType", name="guidedSearch_matrixType"),
    url(r'^storageType/$', "guidedSearch_storageType", name="guidedSearch_storageType"),
    url(r'^selectedEV/$', "guidedSearch_selectedEV", name="guidedSearch_selectedEV"),
    url(r'^eigenvector/$', "guidedSearch_eigenvector", name="guidedSearch_eigenvector"),
    url(r'^schur/$', "guidedSearch_schur", name="guidedSearch_schur"),
    url(r'^cndNumber/$', "guidedSearch_cndNumber", name="guidedSearch_cndNumber"),
    url(r'^singleDouble/$', "guidedSearch_singleDouble", name="guidedSearch_singleDouble"),

    #url(r'^clear_session/$', "eigen_clear_session"),  
    #url(r'^update_session/$', "eigen_update_session"),
)
