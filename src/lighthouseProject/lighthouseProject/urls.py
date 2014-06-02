from django.conf.urls import patterns, include, url

### Uncomment the next two lines to enable the admin:
from django.contrib import admin
admin.autodiscover()


### Configure dajaxice url
from dajaxice.core import dajaxice_autodiscover, dajaxice_config
dajaxice_autodiscover()

### static files
from django.conf import settings
from django.conf.urls.static import static


urlpatterns = patterns('',
    # Examples:
    # url(r'^$', 'lighthouseProject.views.home', name='home'),
    # url(r'^lighthouseProject/', include('lighthouseProject.foo.urls')),

    # Uncomment the admin/doc line below to enable admin documentation:
    # url(r'^admin/doc/', include('django.contrib.admindocs.urls')),

    ### Uncomment the next line to enable the admin:
    url(r'^admin/', include(admin.site.urls)),
    
    ### for dajaxcie
    url(dajaxice_config.dajaxice_url, include('dajaxice.urls')),
    
    ### for dojango
    (r'^dojango/', include('dojango.urls')),

    ### Go to the login page
    #(r'^$', 'django.contrib.auth.views.login'),
    #(r'^index/$', 'django.contrib.auth.views.login'),
    
    ### Link registration/backends/default/urls.py for normal account registration:
    #(r'^accounts/', include('registration.backends.default.urls')),
    
    ### Link emailRegistartion/urls.py to use email as username:
    #(r'^accounts/', include('emailRegistration.urls')),

    ### Link blog/urls.py for blog:
    #(r'^blog/', include('blog.urls')),

    ### Link forum/urls.py for blog:
    #(r'^forum/', include('forum.urls')),
    
    
    ###----------------- for LAPACK -----------------------###
    ### for LAPACK routines -- link lighthouse/urls/lapack_*.py:
    #url(r'^lapack_le/', include('lighthouse.urls.lapack_le')),
    url(r'^lapack_eigen/', include('lighthouse.urls.lapack_eigen')),
    url(r'^lapack_svd/', include('lighthouse.urls.lapack_svd')),
    #url(r'^lapack_eprob/', include('lighthouse.urls.lapack_eprob')),

    ###----------------- for PETSc -----------------------###
    ### for PETSc routines -- link lighthouse/urls/lapack_*.py:
    #url(r'^petsc/', include('lighthouse.urls.petsc_le')),

    ###----------------- for SLEPc -----------------------###
    ### for SLEPc routines -- link lighthouse/urls/lapack_*.py:
    #url(r'^slepc_eprob/', include('lighthouse.urls.slepc_eprob'))
    
) + static(settings.STATIC_URL, document_root=settings.STATIC_ROOT)
