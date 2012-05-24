from django.conf.urls.defaults import *
from Driver.views import *

from Dlighthouse import settings

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
    
 #   (r'^$', welcome),

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
    
    
    # Login / logout.
    #(r'^login/$', 'django.contrib.auth.views.login'),
    #(r'^logout/$', logout_page),
    #(r'^registration/$', registration),
    #(r'^complete/$', complete),
    #
    # Web Search.
    (r'^search/', include('Driver.urls')),

)
