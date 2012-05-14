from django.utils import simplejson
from dajaxice.decorators import dajaxice_register
from dajax.core import Dajax



@dajaxice_register
def createTemplate(request, unique):
	dajax = Dajax()
	for item in unique:
		dajax.script('dojo.create("div", {innerHTML: "%s"}, dojo.byId("div-codeTemp"));' % item)
		
	return dajax.json()
		





