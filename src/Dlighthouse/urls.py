from django.conf.urls.defaults import *
from Dlighthouse.Driver import views
from Dlighthouse import settings



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
    (r'^media/(?P<path>.*)$', 'django.views.static.serve', {'document_root': settings.MEDIA_ROOT}),


### --- The following two lines use Driver.views --- ###
    (r'^search-form/$', views.search_form),
    (r'^search/problem/$', views.search_problem),
    (r'^search/problem/complex/$', views.search_complex), 
    (r'^search/problem/equation/$', views.search_equation),
    (r'^search/problem/equation/complex/$', views.search_complex),
    (r'^search/problem/complex/matrixtype/$', views.search_matrixtype),   
    (r'^search/problem/complex/matrixtype/storage/$', views.search_storage),
    (r'^search/problem/complex/matrixtype/storage/precision/$', views.search_precision),
    (r'^search/$', views.search_result),
    (r'^search-advanced/$', views.search_advanced),
    (r'^advanced/result/$', views.advanced_result),


#    (r'^grid/$', views.grid),
#    (r'^datagrid/$', views.datagrid),
#    (r'^checkbox/$', views.checkbox),
#    (r'^handle/$', views.handle),
)
