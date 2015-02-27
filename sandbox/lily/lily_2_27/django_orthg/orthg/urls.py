from django.conf.urls import patterns, url, include
from orthg import views
from django.contrib import admin
#from django.conf.urls.defaults import *
#from django.views.generic.simple import direct_to_template
from django.conf import settings


urlpatterns = patterns('orthg.views',
    # ex: /polls/
    url(r'^$', views.guidedSearch_index, name='orthg'),
    # ex: /polls/5/
    url(r'^index/$', views.guidedSearch_index, name='orthg'),
    # ex: /polls/5/results/
    url(r'^guidedSearch/$', views.guidedSearch, name='orthg_guidedSearch'),
)
