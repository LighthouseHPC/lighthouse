from django.utils import simplejson
from dajaxice.decorators import dajaxice_register
from dajax.core import Dajax
from dajaxice.core import dajaxice_functions
import os



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
def createTemplate(request, selectedRoutine_ajax, selectedRoutine_session, prog_language):
	dajax = Dajax()
	#dajax.script('console.log("1", selectedRoutineNames)')
	#dajax.script('console.log("2", selectedRoutine_session)')

	# if file already exists, open in append mode, if not open in write mode

	if selectedRoutine_ajax or selectedRoutine_session:
		if prog_language == "C":
			fileName =  './generatedCodeTemplate/' + request.session.session_key + '.c'
		elif prog_language == "FORTRAN":
			fileName =  './generatedCodeTemplate/' + request.session.session_key + '.f'
		
		if os.path.isfile(fileName):
		   	f = open(fileName, 'a')
		else:
			f = open(fileName, 'w')

	# if there is a dnd action, use selectedRoutine_ajax; if not, use selectedRoutine_session.

	if selectedRoutine_ajax:
		#delete the empty string in the selectedRoutineNames list
		for item in filter(None, selectedRoutine_ajax):
			innerHTMLText = "%s(n, nrhs, &a[0][0], lda, ipiv, &b[0][0], ldb, &info)" % item
			dajax.script('dojo.create("div", {innerHTML: "%s"}, dojo.byId("div-codeTemp"));' % innerHTMLText)
			f.write(innerHTMLText+"\n");
	else:
		for item in selectedRoutine_session:
			innerHTMLText = "%s(n, nrhs, &a[0][0], lda, ipiv, &b[0][0], ldb, &info)" % item
			dajax.script('dojo.create("div", {innerHTML: "%s"}, dojo.byId("div-codeTemp"));' % innerHTMLText)
			f.write(innerHTMLText+"\n");

	if selectedRoutine_ajax or selectedRoutine_session:
		f.close()	

	return dajax.json()



@dajaxice_register
def removeTemplateFile(request):
	dajax = Dajax()

	fileName = './generatedCodeTemplate/' + request.session.session_key;
	
	if os.path.isfile(fileName + '.c'):
		os.remove(fileName + '.c')
	elif os.path.isfile(fileName + '.f'):
		os.remove(fileName + '.f')

	return dajax.json()



