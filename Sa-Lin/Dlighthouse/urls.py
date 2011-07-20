from django.conf.urls.defaults import *
from Dlighthouse.Drivers import views




# Uncomment the next two lines to enable the admin:
from django.contrib import admin
admin.autodiscover()


urlpatterns = patterns('',
    # Example:
    # (r'^Dlighthouse/', include('Dlighthouse.foo.urls')),

    # Uncomment the admin/doc line below and add 'django.contrib.admindocs' 
    # to INSTALLED_APPS to enable admin documentation:
    # (r'^admin/doc/', include('django.contrib.admindocs.urls')),

    # Uncomment the next line to enable the admin:
    (r'^admin/', include(admin.site.urls)),
    (r'^dojango/', include('dojango.urls')),
#    (r'^search/', include('haystack.urls')),

### --- The following two lines use Drivers.views --- ###
    (r'^search-form/$', views.search_form),
    (r'^search/problem/$', views.search_problem),
    (r'^search/problem/precision/$', views.search_precision),
    (r'^search/problem/precision/complex/$', views.search_complex), 
    (r'^search/problem/precision/complex/matrixtype/$', views.search_matrixtype),   
    (r'^search/problem/precision/complex/matrixtype/storage/$', views.search_storage),
    #(r'^search/$', views.search_result),
)
