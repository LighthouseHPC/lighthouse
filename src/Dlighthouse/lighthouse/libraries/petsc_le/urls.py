from django.conf.urls.defaults import *
from django.conf import settings


urlpatterns = patterns('lighthouse.views.petsc_le',
    url(r'^$', "petsc", name="petsc"),
    url(r'^linear_system/$', "linear_system"),
    url(r'^linear_system/generateCode/$', "petsc_code"),
    url(r'^linear_system/generateCode/download/$', "downloadCode"),
)
