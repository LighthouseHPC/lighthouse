from django.conf.urls.defaults import *
from django.conf import settings


urlpatterns = patterns('lighthouse.views.btoscript',
    url(r'^$', 'index'),
    url(r'^args/$', 'args'),
#    url(r'^codes/$', 'codes'),
)

