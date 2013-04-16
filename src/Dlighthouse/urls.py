from django.conf.urls.defaults import *
from Dlighthouse import settings


### Uncomment the next two lines to enable the admin:
from django.contrib import admin
admin.autodiscover()

### To enable dajaxice
from dajaxice.core import dajaxice_autodiscover, dajaxice_config
dajaxice_autodiscover()
from django.contrib.staticfiles.urls import staticfiles_urlpatterns


urlpatterns = patterns('',
    ### Uncomment the admin/doc line below and add 'django.contrib.admindocs' 
    ### to INSTALLED_APPS to enable admin documentation:
    # (r'^admin/doc/', include('django.contrib.admindocs.urls')),

    ### For dojango
    #(r'^dojango/', include('dojango.urls')),

    ### For Haystack
    #(r'^search/', include('haystack.urls')),
    
    
    ###  For dajaxice
    url(dajaxice_config.dajaxice_url, include('dajaxice.urls')),

    ### Uncomment the next line to enable the admin:
    (r'^admin/', include(admin.site.urls)),

    ### Use the files in the media directory:
    (r'^media/(?P<path>.*)$', 'django.views.static.serve', {'document_root': settings.MEDIA_ROOT}),
    
    ### Use the files in the templates dirctory
    (r'^templates/(?P<path>.*)$', 'django.views.static.serve', {'document_root': settings.TEMPLATE_ROOT}),
    
    ### Go to the login page
    (r'^$', 'django.contrib.auth.views.login'),
    (r'^index/$', 'django.contrib.auth.views.login'),
    
    ### Link registration/backends/default/urls.py for normal account registration:
    #(r'^accounts/', include('registration.backends.default.urls')),
    
    ### Link emailRegistartion/urls.py to use email as username:
    (r'^accounts/', include('emailRegistration.urls')),

    ### Link blog/urls.py for blog:
    (r'^blog/', include('blog.urls')),

    ### Link forum/urls.py for blog:
    (r'^forum/', include('forum.urls')),
    
    ### Link lighthouse/library/lapack_le/urls.py for guided, advanced, and keyword Searches:
    (r'^lapack_le/', include('lighthouse.libraries.lapack_le.urls')),

    ### Link lighthouse/library/petsc/urls.py for guided, advanced, and keyword Searches:
    (r'^petsc/', include('lighthouse.libraries.petsc.urls')),


)

urlpatterns += staticfiles_urlpatterns()

