from django.conf.urls.defaults import *
from django.views.generic.simple import direct_to_template
from django.conf import settings

urlpatterns = patterns('lighthouse.views.slepc_eprob',
	url(r'^$', "slepc_eprob", name="slepc_eprob"),
	url(r'^guided/class$', "guidedSearch_class"),
	url(r'^guided/eps$', "slepc_eprob_eps"),
	url(r'^guided/pep$', "slepc_eprob_pep"),
	url(r'^guided/pep/degree$', "slepc_eprob_degree_pep"),
	url(r'^guided/pep/misc$', "slepc_eprob_misc_pep"),
    url(r'^guided/eps/generateCode/$', "generateTemplate"),
    url(r'^update_session/$', "update_slepc_session"),
    url(r'^clear_session/$', "clear_session"),
    url(r'^remove_session/$', "remove_session"),
    
)
