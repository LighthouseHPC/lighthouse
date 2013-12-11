from django.conf.urls import patterns, include, url

### Uncomment the next two lines to enable the admin:
from django.contrib import admin
admin.autodiscover()

urlpatterns = patterns('',
    # Examples:
    # url(r'^$', 'lighthouseProject.views.home', name='home'),
    # url(r'^lighthouseProject/', include('lighthouseProject.foo.urls')),

    # Uncomment the admin/doc line below to enable admin documentation:
    # url(r'^admin/doc/', include('django.contrib.admindocs.urls')),

    ### Uncomment the next line to enable the admin:
    url(r'^admin/', include(admin.site.urls)),

    ### Link lighthouse/library/lapack_eigen/urls.py for guided, advanced, and keyword Searches:
    url(r'^lapack_eigen/', include('lighthouse.libraries.lapack_eigen.urls')),
)
