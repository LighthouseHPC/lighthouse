from django.utils import simplejson
from dajaxice.decorators import dajaxice_register
from dajax.core import Dajax



@dajaxice_register
def selectedRoutines(request):
	dajax = Dajax()	
	#if request.method == 'POST':
	routineCheckbox = request.POST.getlist('routineCheckbox')
	#for item in routineCheckbox:
		#dajax.script('dojo.create("li", {innerHTML: "%s"}, dojo.byId("selectedListNode"));' % item)
	dajax.script('console.log("%s");' % routineCheckbox)
	return dajax.json()



@dajaxice_register
def createTemplate(request, selectedRoutineNames):
	dajax = Dajax()
	#delete the empty string in the selectedRoutineNames list
	for item in filter(None, selectedRoutineNames):
		innerHTMLText = "%s(n, nrhs, &a[0][0], lda, ipiv, &b[0][0], ldb, &info)" % item
		dajax.script('dojo.create("div", {innerHTML: "%s"}, dojo.byId("div-codeTemp"));' % innerHTMLText)
		
	return dajax.json()
		


