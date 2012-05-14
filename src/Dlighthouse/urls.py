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
    (r'^search/problem/$', views.search_problem), 
    (r'^search/problem/equation/$', views.search_equation),
    (r'^search/problem/equation/factor/$', views.search_factor),
    (r'^search/problem/equation/factor/complex$', views.search_complex),
    (r'^search/problem/complex/$', views.search_complex),
    (r'^search/problem/complex/matrixtype/$', views.search_matrixtype),   
    (r'^search/problem/complex/matrixtype/storage/$', views.search_storage),
    (r'^search/problem/complex/matrixtype/storage/precision/$', views.search_precision),

    (r'^advanced-search/$', views.advancedsearch),
    (r'^advanced/form/$', views.advancedsearchform),
    (r'^advanced/result/$', views.advancedresult),
)
