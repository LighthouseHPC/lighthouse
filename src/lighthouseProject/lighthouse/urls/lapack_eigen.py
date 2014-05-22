from django.conf.urls.defaults import *
from django.views.generic.simple import direct_to_template
from django.conf import settings

urlpatterns = patterns('lighthouse.views.lapack_eigen',
    url(r'^$', "guidedSearch_index", name="guidedSearch_index"),
    url(r'^index/$', "guidedSearch_index", name="guidedSearch_index"),
    url(r'^guidedSearch/$', "guidedSearch", name="guidedSearch"),

    #url(r'^clear_session/$', "eigen_clear_session"),  
    #url(r'^update_session/$', "eigen_update_session"),
)
