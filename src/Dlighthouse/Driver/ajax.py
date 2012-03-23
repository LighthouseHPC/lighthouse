from django.utils import simplejson
from dajax.core import Dajax
from dajaxice.decorators import dajaxice_register
from forms import *
from views import *


###--------- Simple Search: Help Info -------------### 
@dajaxice_register
def linearEquationHelp(request):
	return simplejson.dumps({'message': 'A linear equation is...'})








####--------- Simple Search: Form Update ------------ ###
#def id_to_form(formId):
#	formDict = {'question_prob': ProblemForm(), 'question_equa': EquationForm(), 'question_fact': FactorForm(), 'question_comp': ComplexForm, 'question_type': MatrixTypeForm, 'question_stor': StorageForm, 'question_prec': PrecisionForm, }
#	return formDict[formId] 
#
#
#
#
#@dajaxice_register
#def formUpdate(request, formId, selectedValues, ulId, nextFormId, nextFormDiv):
#	dajax = Dajax()
#	## clean the options
#	dajax.clear('#%s ol' % formId, 'innerHTML')
#	dajax.clear('#%s ul' % formId, 'innerHTML')
#
#	## post the answers
#	form = id_to_form(formId)
#	choices = form.fields[formId].choices
#	answers = []
#	for item in choices:
#		if item[0] in selectedValues:
#			answers.append(item[1])
#			dajax.script('dojo.create("li",{innerHTML: "%s"}, dojo.byId("%s"));' % (item[1], ulId))
#	
#	## load the next form
#	if nextFormId == 'question_equa':
#		for item in answers:
#			if item in ['Solve a linear equation only', 'Refine the solution', 'Compute forward or backward error bounds for the solution', 'Estimate the condition number of the matrix', 'Equilibrate a matrix']:
#				nextForm = id_to_form(nextFormId)
#			else:
#				nextForm = ComplexForm()
#				nextFormDiv = 'div_comp'
#
#	else:  
#		nextForm = id_to_form(nextFormId)
#	dajax.append('#%s'% nextFormDiv, 'innerHTML', nextForm.as_p())
#
#	## load the search results
##	render = search_problem(request)
##	dajax.assign('#simpleResult', 'innerHTML', render)
#	return dajax.json()






