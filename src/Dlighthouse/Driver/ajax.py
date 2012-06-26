from django.utils import simplejson
from dajaxice.decorators import dajaxice_register
from dajax.core import Dajax



#@dajaxice_register
#def selectedRoutines(request):
#	dajax = Dajax()	
#	#if request.method == 'POST':
#	routineCheckbox = request.POST.getlist('routineCheckbox')
#	#for item in routineCheckbox:
#		#dajax.script('dojo.create("li", {innerHTML: "%s"}, dojo.byId("selectedListNode"));' % item)
#	dajax.script('console.log("%s");' % routineCheckbox)
#	return dajax.json()



@dajaxice_register
def createTemplate(request, selectedRoutine_ajax, selectedRoutine_session):
	dajax = Dajax()
	#dajax.script('console.log("1", selectedRoutineNames)')
	#dajax.script('console.log("2", selectedRoutine_session)')

	# if there is a dnd action, use selectedRoutine_ajax; if not, use selectedRoutine_session.
	if selectedRoutine_ajax:
		#delete the empty string in the selectedRoutineNames list
		for item in filter(None, selectedRoutine_ajax):
			innerHTMLText = "%s(n, nrhs, &a[0][0], lda, ipiv, &b[0][0], ldb, &info)" % item
			dajax.script('dojo.create("div", {innerHTML: "%s"}, dojo.byId("div-codeTemp"));' % innerHTMLText)
	else:
		for item in selectedRoutine_session:
			innerHTMLText = "%s(n, nrhs, &a[0][0], lda, ipiv, &b[0][0], ldb, &info)" % item
			dajax.script('dojo.create("div", {innerHTML: "%s"}, dojo.byId("div-codeTemp"));' % innerHTMLText)
				
	return dajax.json()