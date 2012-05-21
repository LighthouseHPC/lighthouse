from django.conf.urls.defaults import *
from django.conf import settings

from Dlighthouse import settings
from Dlighthouse.Driver import views

# Uncomment the next two lines to enable the admin:
from django.contrib import admin
admin.autodiscover()

# To enable dajaxice
from dajaxice.core import dajaxice_autodiscover
dajaxice_autodiscover()


urlpatterns = patterns('',
    # Example:
    # (r'^Dlighthouse/', include('Dlighthouse.foo.urls')),

    # Uncomment the admin/doc line below and add 'django.contrib.admindocs' 
    # to INSTALLED_APPS to enable admin documentation:
    # (r'^admin/doc/', include('django.contrib.admindocs.urls')),

    # Uncomment the next line to enable the admin:
    (r'^admin/', include(admin.site.urls)),

    #  dajaxice URLS
    (r'^%s/' % settings.DAJAXICE_MEDIA_PREFIX, include('dajaxice.urls')),

    # For dojango
    (r'^dojango/', include('dojango.urls')),

    # For Haystack
    (r'^search/', include('haystack.urls')),

    (r'^media/(?P<path>.*)$', 'django.views.static.serve', {'document_root': settings.MEDIA_ROOT}),
    (r'^templates/(?P<path>.*)$', 'django.views.static.serve', {'document_root': settings.TEMPLATE_ROOT}),


### --- The following lines use Driver.views --- ###
    (r'^index/$', views.search_forms),
    (r'^guided/problem/$', views.guidedSearch_problem), 
    (r'^guided/problem_equation/$', views.guidedSearch_equation),
    (r'^guided/problem_equation_factor/$', views.guidedSearch_factor),
    (r'^guided/problem_equation_factor_complex$', views.guidedSearch_complex),
    (r'^guided/problem_complex/$', views.guidedSearch_complex),
    (r'^guided/problem_complex_matrixtype/$', views.guidedSearch_matrixtype),   
    (r'^guided/problem_complex_matrixtype_storage/$', views.guidedSearch_storage),
    (r'^guided/problem_complex_matrixtype_storage_precision/$', views.guidedSearch_precision),

    #(r'^advanced/$', views.advancedSearch),
    (r'^advanced/form/$', views.advancedForm),
    (r'^advanced/result/$', views.advancedResult),
)
