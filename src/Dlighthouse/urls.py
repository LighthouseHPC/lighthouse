from django.conf.urls.defaults import *
from Dlighthouse import settings


### Uncomment the next two lines to enable the admin:
from django.contrib import admin
admin.autodiscover()

### To enable dajaxice
from dajaxice.core import dajaxice_autodiscover, dajaxice_config
dajaxice_autodiscover()
from django.contrib.staticfiles.urls import staticfiles_urlpatterns

### Straight to template
from django.views.generic import TemplateView


urlpatterns = patterns('',
    ### Uncomment the admin/doc line below and add 'django.contrib.admindocs' 
    ### to INSTALLED_APPS to enable admin documentation:
    # (r'^admin/doc/', include('django.contrib.admindocs.urls')),

    ### For dojango
    (r'^dojango/', include('dojango.urls')),

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
    url(r'^$', 'django.contrib.auth.views.login', name="login"),
    url(r'^login/$', 'django.contrib.auth.views.login', name="login"),

    ### Go to home page
    url(r'^index/$', TemplateView.as_view(template_name='home.html'), name="home"),
    url(r'^home/$', TemplateView.as_view(template_name='home.html'), name="home"),
    
    ### Link registration/backends/default/urls.py for normal account registration:
    #(r'^accounts/', include('registration.backends.default.urls')),
    
    ### Link emailRegistartion/urls.py to use email as username:
    (r'^accounts/', include('emailRegistration.urls')),

    ### Link blog/urls.py for blog:
    (r'^blog/', include('blog.urls')),

    ### Link forum/urls.py for blog:
    (r'^forum/', include('forum.urls')),
    
    ###----------------- for LAPACK -----------------------###
    ### for LAPACK routines -- link lighthouse/urls/lapack_*.py:
    url(r'^lapack_le/', include('lighthouse.urls.lapack_le')),
    url(r'^lapack_eigen/', include('lighthouse.urls.lapack_eigen')),
    url(r'^lapack_svd/', include('lighthouse.urls.lapack_svd')),
    url(r'^lapack_sylvester/', include('lighthouse.urls.lapack_sylvester')),
    url(r'^lapack_eprob/', include('lighthouse.urls.lapack_eprob')),

    ###----------------- for PETSc -----------------------###
    ### for PETSc routines -- link lighthouse/urls/lapack_*.py:
    url(r'^petsc/', include('lighthouse.urls.petsc_le')),

    ###----------------- for SLEPc -----------------------###
    ### for SLEPc routines -- link lighthouse/urls/lapack_*.py:
    url(r'^slepc_eprob/', include('lighthouse.urls.slepc_eprob')),

    ###----------------- for BTO script ------------------###
    ### ###
    url(r'^btoscript/', include('lighthouse.urls.btoscript'))

)

urlpatterns += staticfiles_urlpatterns()