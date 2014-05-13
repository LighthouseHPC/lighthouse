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

    ### Link lighthouse/library/lapack_eigen/urls.py for guided, advanced, and keyword Searches:
    url(r'^lapack_eigen/', include('lighthouse.urls.lapack_eigen')),
    
) + static(settings.STATIC_URL, document_root=settings.STATIC_ROOT)
