import string, types, sys, os, StringIO, re, shlex, json, zipfile
from collections import OrderedDict
from django.contrib.auth.decorators import login_required
from django.core.servers.basehttp import FileWrapper
from django.http import HttpResponse, HttpResponseNotFound
from django.shortcuts import render_to_response, redirect, render
from django.template import RequestContext
from django.template.loader import render_to_string
from django.views.decorators.csrf import csrf_exempt
#from django.utils import simplejson 
from orthg.models import least
from orthg.choiceDict import *
from orthg.forms import *
from haystack.forms import ModelSearchForm
from spell.spell import correct
from haystack.query import SearchQuerySet
from haystack.inputs import AutoQuery, Exact, Clean
from haystack.views import SearchView
from itertools import chain





import datetime

form_order_standard = ['standardGeneralizedForm', 'complexNumberForm', 'FullStorageForm', 'sFullRankForm','singleDoubleForm']
form_order_qr= [ 'sFullRankForm','qrForm', 'singleDoubleForm']
form_order_svd= [ 'sFullRankForm','svdForm', 'singleDoubleForm']
form_order_generalized = ['standardGeneralizedForm', 'complexNumberForm', 'FullStorageForm', 'gFullRankForm', 'singleDoubleForm']
form_2arguments = ['FullStorageForm']
form_HTML = ['standardGeneralizedForm']   

### help functions
def question_and_answer(form, value, choices):
    for field in form:
        question = unicode(field.label)
    for choice in choices:
        if choice[0] == value:
            answer = choice[1]
    return {question: [answer]}

def find_nextForm(currentForm_name, request):
    print request.session.get('form_order')
    current_index = request.session.get('form_order').index(currentForm_name)
    nextForm_name = ""        
    nextForm = ""
    
    try: 
        ## search for 'none' and return the first column that has zero to be the next question/form
        next_index = next(i for i in range(current_index+1, len(request.session.get('form_order'))))
        nextForm_name = request.session.get('form_order')[next_index]
        print nextForm_name
        nextForm = getattr(sys.modules[__name__], nextForm_name)()
                #nextForm = getattr(sys.modules[__name__], nextForm_name)()
    ## the end of the guided search or other errors
    except Exception as e:          
        print type(e)
        print "e.message: ", e.message
        print "e.args: ", e.args
    
    return {'nextForm_name': nextForm_name, 'nextForm': nextForm}

### start guided search views
def guidedSearch_index(request):
    ## set up session keys and values
    for item in ['standardGeneralizedForm', 'complexNumberForm', 'FullStorageForm', 'sFullRankForm', 'qrForm', 'svdForm', 'gFullRankForm', 'singleDoubleForm']:
        key = 'orthg_'+item[:-4]
        request.session[key] = ''    
    request.session['currentForm_name'] = 'standardGeneralizedForm'
    request.session['Routines'] = least.objects.all()
    request.session['orthg_guided_answered'] = OrderedDict()
    
    ## get ready for the template
    context = {
                'formHTML': "standardGeneralizedForm",
                'form': "invalid",
                'orthg_guided_answered' : '',
                'results' : 'start'
    }
    return render_to_response('orthg/index.html', context_instance=RequestContext(request, context))

def guidedSearch(request):
## distinguish forms that take 2 arguments from forms that take 1 argument
    form = getattr(sys.modules[__name__], request.session['currentForm_name'])(request.GET or None)
    if form.is_valid():
        ## get current question and user's answer
        current_question = request.session.get('currentForm_name')[:-4]
        formField_name = 'orthg_'+current_question
        value = form.cleaned_data[formField_name]
        choices = form.fields[formField_name].choices        
        request.session.get('orthg_guided_answered').update(question_and_answer(form, value, choices))
 
        ## if user chooses to stop the search, start over; otherwise, perform the search
        if value == 'stop':
            return guidedSearch_index(request)
        else:        
            ## do search based on user's response
            lookup = "%s__contains" % current_question
            query = {lookup : value}
            request.session['Routines'] = request.session.get('Routines').filter(**query)
                                    
            ## generate a session for current question/answer -->request.session.get(eigen_currentQuestion) = answer
            request.session[formField_name] = value
            
              ## decide which form order to use
            if request.session['currentForm_name'] == 'standardGeneralizedForm' and request.session.get('orthg_standardGeneralized') == 'standard':
               request.session['form_order'] = form_order_standard
            elif request.session['currentForm_name'] == 'standardGeneralizedForm' and request.session.get('orthg_standardGeneralized') == 'generalized':               request.session['form_order'] = form_order_generalized     
            #elif request.session['currentForm_name'] == 'sFullRankForm' and request.session.get('orthg_sFullRank') == 'no':
             #    request.session['form_order'] = form_order_qr
            #elif request.session['currentForm_name'] == 'sFullRankForm' and request.session.get('orthg_sFullRank') == 'yes':
            #     request.session['form_order'] = form_order_svd
      
             ## call function find_nextForm to set up next form for next question
            if request.session['orthg_FullStorage'] == 'no':         ## stop search
               return guidedSearch_index(request)
            else:
                 dict_nextQuestion = find_nextForm(request.session.get('currentForm_name'), request)           
                 nextForm_name = dict_nextQuestion['nextForm_name']
                 nextForm = dict_nextQuestion['nextForm']
            ## make next form current for request.session.get('currentForm_name')
                 request.session['currentForm_name'] = nextForm_name 
            
            ## decide whether or not to use form HTML files (if help buttons are needed, use HTML file instead of form)
                 if nextForm_name in form_HTML:
                     formHTML = nextForm_name
                 else:
                      formHTML = "invalid"
            
            ## get ready for the template       
                 context = {
                              'formHTML': formHTML,
                              'form': nextForm,
                              'orthg_guided_answered' : request.session.get('orthg_guided_answered'),
                              'results' : request.session.get('Routines')
                            }
                 return render_to_response('orthg/index.html', context_instance=RequestContext(request, context))
    else:       
        print form.error
        return guidedSearch_index(request)

###---------------- Keyword Search ------------------###
special_words = {
		'complexNumber': ['no', 'yes'],
		'thePrecision': ['single', 'double'],
		'matrixType': ['general', 'symmetric', 'Hermitian', 'SPD', 'HPD', 'symmetric positive definite', 'Hermitian positive definite', 'triangular', 'SPsD', 'HPsD', 'symmetric positive semidefinite', 'Hermitian positive semidefinite'],
		'storageType': ['full', 'band', 'packed', 'tridiagonal', 'rectangular full packed', 'RFP'],
                'standardGeneralized': ['standard','generalized'],
                'gFullRank':['Both are Full Rank','Only A is Full Rank','Only B is Full Rank'],
                'sFullRank':['Full Rank','Not Full Rank'],
                'qr':['QR','Faster QR'],
                'svd':['SVD','divide and conquer'],
		'table': ['factor', 'factorization', 'condition number', 'error bound', 'equilibrate', 'inverse', 'driver', 'expert', 'computational', 'solve', 'refine','least'],
	}


def keywordResult(request):
	modelList = []
	answer = []
	keywords_dictionary = {}
	keywords_origList = []
	keywordsList = []
	keywords = ""

	try:
		request.session['selectedRoutines']
		request.session['scriptOutput']
		request.session['userScript']	
	except (NameError,KeyError):
		request.session['selectedRoutines'] = []
		request.session['scriptOutput'] = ""
		request.session['userScript'] = ""
	
	if request.method == 'POST':		
		form = ModelSearchForm(request.POST)
		#print form
		
		if form.is_valid():
			## if driver/computational boxes are checked
			#answer_class = form.cleaned_data['models']
				
			## get the keyword
			keywords_orig = request.POST['q']
			
			## Don't split double-quoted words ##
			keywords_origList = shlex.split(keywords_orig)
			
			## split all words ##
			keywords_singleList = keywords_orig.split()
			
			## spell check ##
			for i, item in enumerate(keywords_singleList):
				keywords_singleList[i] = spell_check(item)	
				
			## make a string out of keywordsList ##
			keywords = " ".join(keywords_singleList)
			
			## keywords goes through keyword_handler ##
			keywords = keyword_handler(keywords)
			#print keywords
			
			## final keywordsList, Don't split double-quoted words
			keywordsList = shlex.split(keywords)
			
			## find the words that are not corrected ##
			common = list(set(keywords_origList) & set(keywordsList))
			#print common
			
			
			###***** make a dictionary for the keywords for django query *****###
			sumList = []
			for key in special_words:
				keywords_dictionary[key] = list(set(keywordsList) & set(special_words[key]))
				sumList += keywords_dictionary[key]
			keywords_dictionary['other'] = list(set(keywordsList) - set(sumList))
			
			## keep 'transpose' and 'conjugate transpose' only
			keywords_dictionary['other'] = list(set(['transpose', 'conjugate transpose']) & set(keywords_dictionary['other']))
			print keywords_dictionary
			
			if not any([keywords_dictionary[i] == [] for i in ['table', 'matrixType']]):
				print 'use django'
				keywords_dictionary = keyword_handler2(keywords_dictionary)
				keywords_dictionary = kwDictionary_set(keywords_dictionary)
				#print keywords_dictionary
				results = query_django(keywords_dictionary)				
			else:
				print 'use haystack'
				results = SearchQuerySet().models(least).filter(content=AutoQuery(keywords)).order_by('id')
							
			
			context = {'results': results,
				   'keywordsList': keywordsList,
				   'common': common,
				   'selectedRoutines': request.session['selectedRoutines'],
				   #'notSelectedRoutines': request.session['notSelectedRoutines'],
				   }
			
			return render_to_response(
				'orthg/keywordResult.html', 
				{'KeywordTab': True}, 
				context_instance=RequestContext(request, context)
			)
		else:
			HttpResponse("Error!")
			
			
			

#def quoted_words(string):
#	matches=re.findall(r'\"(.+?)\"', string)
#	return matches
	


def spell_check(word):
	word = re.sub(r'\b.*?qu.*?lib.*?\b', 'equilibrate', word)
	if word.lower() in ['spd', 'hpd', 'lu', 'qr', 'rfp']:
		word = word.upper()
	elif word.lower() == 'spsd':
		word = 'SPsD'
	elif word.lower() == 'hpsd':
		word = 'HPsD'		
	else: 	
		word= correct(word)
	return word


	
def keyword_handler(strings):
	strings = re.sub(r'\bli.*? eq.*?\b', 'linear equations', strings)
	strings = re.sub(r'\blinear equations s.*?ver\b', 'linear equations solver', strings)
	strings = re.sub(r'\bsys.*? linear eq.*?\b', 'system of linear equations', strings)
	strings = re.sub(r'\berror b.*?\b', '\"'+'error bounds'+'\"', strings)
	strings = re.sub(r'\bcondition n.*?\b', '\"'+'condition number'+'\"', strings)
	strings = re.sub(r'\bLU fact.*?\b', '\"'+'LU factorization'+'\"', strings)
	strings = re.sub(r'\bCh.*?ky fact.*?\b', '\"'+'Cholesky factorization'+'\"', strings)
	strings = re.sub(r'\bLU decomp.*?\b', '\"'+'LU decomposition'+'\"', strings)
	strings = re.sub(r'\bequilib.*?\b', 'equilibrate', strings)
	#strings = re.sub(r'\binv.*?t.*?\b', 'invert', strings)
	strings = re.sub(r'\binvert*?\b', 'inverse', strings)
	strings = re.sub(r'\bhermitian.*?\b', 'Hermitian', strings)
	strings = re.sub(r'\bHermitian p.*? def.*?\b', '\"'+'Hermitian positive definite'+'\"', strings)
	strings = re.sub(r'\bHermitian p.*? semidef.*?\b', '\"'+'Hermitian positive semidefinite'+'\"', strings)
	strings = re.sub(r'\bsymmetric.*?\b', 'symmetric', strings)
	strings = re.sub(r'\bsymmetric p.*? def.*?\b', '\"'+'symmetric positive definite'+'\"', strings)
	strings = re.sub(r'\bsymmetric p.*? semidef.*?\b', '\"'+'symmetric positive semidefinite'+'\"', strings)
	strings = re.sub(r'\bband.*?\b', 'band', strings)
	strings = re.sub(r'\bpack.*?\b', 'packed', strings)
	strings = re.sub(r'\brectang.*?\b fu.*? pa.*?\b', '\"'+'rectangular full packed'+'\"', strings)
	strings = re.sub(r'\gauss.*? elim.*?\b', 'Gaussian elimination', strings)
	strings = re.sub(r'\conj.*? tra.*?\b', '\"'+'conjugate transpose'+'\"', strings)
	return strings	






'''###------- set up keywords_dictionary to use proper model names--------###
table_handler2 = {
	'factorization': 'factor',
	'condition number': 'condition_number',
	'error bound': 'error_bound',
	'refine': 'error_bound',
	'refinement': 'error_bound',
}

matrixType_handler2 = {
	'symmetric positive definite': 'SPD',
	'Hermitian positive definite': 'HPD',
	'symmetric positive semidefinite': 'SPsD',
	'Hermitian positive semidefinite': 'HPsD',
}
def keyword_handler2(keywords_dictionary):
	## change table name
	for i, item in enumerate(keywords_dictionary['table']):
		for key, value in table_handler2.iteritems():
			if item == key:
				keywords_dictionary['table'][i] = value
				
	## change matrix type name
	for i, item in enumerate(keywords_dictionary['matrixType']):
		for key, value in matrixType_handler2.iteritems():
			if item == key:
				keywords_dictionary['matrixType'][i] = value

	## change storage type name
	for i, item in enumerate(keywords_dictionary['storageType']):
		if item == 'rectangular full packed':
			keywords_dictionary['storageType'][i] = 'RFP'

				
	## for 'solve a system of linear equations'
	if len(keywords_dictionary['table']) == 1 and 'solve' in keywords_dictionary['table']:
		keywords_dictionary['table'] = ['only', 'expert']
		
	elif 'driver' in (keywords_dictionary['table']) and 'solve' in keywords_dictionary['table']:
		keywords_dictionary['table'] = ['driver']
		
	elif 'computational' in (keywords_dictionary['table']) and 'solve' in keywords_dictionary['table']:
		keywords_dictionary['table'] = ['solve']
		
	elif len(keywords_dictionary['table'])>1 and 'solve' in keywords_dictionary['table']:
		keywords_dictionary['table'] = ['expert']
		
	else:
		pass	
	
	
	return keywords_dictionary'''	





###------- set up keywords_dictionary for query_django--------###
def kwDictionary_set(keywords_dictionary):		
	###***** convert table strings to class *****###
	'''for i,value in enumerate(keywords_dictionary['table']):	
		value = "lapack_le_"+value
		tableClass = ContentType.objects.get(model=value).model_class()
		keywords_dictionary['table'][i] = tableClass'''
		
	if keywords_dictionary['storageType']==[]:
		keywords_dictionary['storageType'] = ['full']
		
	###***** combine matrixType and storageType *****###
	keywords_dictionary.update({'matrix_storage':[]})
	for matrix in keywords_dictionary['matrixType']:
		for storage in keywords_dictionary['storageType']:
			combineType = matrix+"_"+storage
			keywords_dictionary['matrix_storage'].append(combineType)
	
	###***** delete keywords_dictionary['matrixType'] and keywords_dictionary['storageType'] *****###
	del keywords_dictionary['matrixType']
	del keywords_dictionary['complexNumber']
	
	
	###***** combine dataType and thePrecision to determine 's', 'd', 'c', 'z'. *****###
	precisionList = []
	if len(keywords_dictionary['complexNumber']) == 0 and len(keywords_dictionary['thePrecision']) != 0:
		for precision in keywords_dictionary['thePrecision']:
			if precision =='single':
				precisionList.extend(['s', 'c'])
			else:
				precisionList.extend(['d', 'z'])
	elif len(keywords_dictionary['complexNumber']) != 0 and len(keywords_dictionary['thePrecision']) == 0:
		for data in keywords_dictionary['complexNumber']:
			if data == 'no':
				precisionList.extend(['s', 'd'])
			else:
				precisionList.extend(['c', 'z'])
	elif len(keywords_dictionary['complexNumber']) == 0 and len(keywords_dictionary['thePrecision']) == 0:
		precisionList.extend(['s', 'd', 'c', 'z'])
	else:
		for data in keywords_dictionary['complexNumber']:
			for precision in keywords_dictionary['thePrecision']:
				if data == 'no' and precision =='single':
					precision = 's'
				elif data == 'no' and precision =='double':
					precision = 'd'
				elif data == 'yes' and precision =='single':
					precision = 'c'
				else:
					precision = 'z'
				precisionList.append(precision)
	keywords_dictionary['thePrecision'] = precisionList
			
	###***** delete keywords_dictionary['dataType'] *****###
	del keywords_dictionary['complexNumber']
	return keywords_dictionary





'''###-------- Django querry search --------###
def query_django(keywords_dictionary):
	results = []
	for table in keywords_dictionary['table']:
		#print table
		for combineType in keywords_dictionary['matrix_storage']:
			for precision in keywords_dictionary['thePrecision']:
				kwargs = {'matrixType': combineType.split('_')[0],
					'storageType': combineType.split('_')[1],
					'thePrecision': precision}
				if keywords_dictionary['other'] == []:
					results += table.objects.filter(**kwargs).order_by('id')
				else:
					for item in keywords_dictionary['other']:
						results += table.objects.filter(**kwargs).filter(notes__icontains=item).order_by('id')
	return results'''










###---------------- for Selected and NotSelected----------------###
"""
From the list of routines returned after each step of the Guided Search (i.e. request.session['Routines']), 
this function creates a new list of routines that excludes the routines 
that are in the request.session['selectedRoutines'] list
"""
def filterSelectedRoutines(request):	
	request.session['notSelectedRoutines'] = request.session['Routines']

	for item in request.session['selectedRoutines']:
		request.session['notSelectedRoutines'] = request.session['notSelectedRoutines'].exclude(Q(thePrecision=item['thePrecision']), Q(routineName=item['routineName']))	
	
	request.session.modified = True




"""
From the list of routines returned by a Keyword Search this function removes 
the routines that are in the request.session['selectedRoutines'] list
"""
def filterSelectedRoutines2(request, routines): 
	indices = []
	i = 0
	for item1 in routines:		
		for item2 in request.session['selectedRoutines']:
			# Save the indices of the routines that match
			if item2['thePrecision'] == item1.thePrecision and item2['routineName'] == item1.routineName:
				indices.append(i)
		i += 1

	# Reverse the list, so the routine with highest index gets popped first 
	# (popping the lowest index first messes up the list)
	indices.reverse()

	for item in indices:
		routines.pop(item)
	
	return routines



"""
From the list of routines returned by an Advanced Search this function  
creates a new list of routines (in the same format as the search result)
that contains only the routines that are in request.session['selectedRoutines'] list
"""
def filterSelectedRoutines3(request, routines):
	alreadySelectedRoutines = []

	for item1 in request.session['selectedRoutines']:
		for lst in routines:
			for item2 in lst:
				if item2.thePrecision == item1['thePrecision'] and item2.routineName == item1['routineName']:
					alreadySelectedRoutines.append(item2)

	return alreadySelectedRoutines







###---------------- Script ------------------###

@csrf_exempt
def runScript(request):
	
	code = request.POST.get('scriptCode')

	if code == "":
		request.session['userScript'] = ""
		request.session['scriptOutput'] = ""
		output = ""
	else:
		bto = BTOGenerator()
		output = bto.generateCode(str(code))		
		request.session['userScript'] = code
		request.session['scriptOutput'] = output

	request.session.modified = True

	return HttpResponse(output)






###---------------- Ajax post to update request.session['selectedRoutines']------------------###
@csrf_exempt
def update_session(request):
	if request.is_ajax():
		selectedRoutineNames = []
		selectedRoutineList = [{
			"thePrecision": request.POST.get('precision'),
			"routineName": request.POST.get('routineName'),
			"standardGeneralized": request.POST.get('standardGeneralized'),
			#"storageType": request.POST.get('storageType'),
			"id": request.POST.get('idn'),
			"url": request.POST.get('url'),
			#"checkState": request.POST.get('checkState')
		}]
		
		if selectedRoutineList[0] not in request.session['selectedRoutines']:
			request.session['selectedRoutines'] = request.session['selectedRoutines'] + selectedRoutineList

		return HttpResponse('Dropped '+request.POST.get('precision')+request.POST.get('routineName'))
		
		## Check if the routine already exists in request.session['selectedRoutines'], if it does save it's index
		#counter = 0
		#match = -1
		#for item in request.session['selectedRoutines']:
		#	if item['thePrecision'] == selectedRoutineList[0]['thePrecision'] and item['routineName'] == selectedRoutineList[0]['routineName']:
		#		match = counter # Save the index
		#		if selectedRoutineList[0]['checkState'] == 'checked':
		#			request.session['selectedRoutines'][counter]['checkState'] = 'checked'
		#		if selectedRoutineList[0]['checkState'] == 'unchecked':
		#			request.session['selectedRoutines'][counter]['checkState'] = 'unchecked'							
		#	counter += 1
		#
		#if match == -1: # The routine does not exist in request.session['selectedRoutines'], so add it
		#	request.session['selectedRoutines'] = request.session['selectedRoutines'] + selectedRoutineList
		#
		## Session was modified
		#request.session.modified = True
		#
		## Create a list of all checked routines	
		#for item in request.session['selectedRoutines']:
		#	if item['checkState'] == 'checked':
		#		selectedRoutineNames.append(item['thePrecision']+item['routineName']+',')
		#
		## Return the list
		#return HttpResponse(selectedRoutineNames)
	else:
		return HttpResponse('only AJAX requests are allowed!')






###---------------- Ajax post to clear request.session['selectedRoutines']------------------###
@csrf_exempt
def clear_session(request):
	if request.is_ajax():
		request.session['selectedRoutines'] = []
		return HttpResponse('All cleared!')		
	else:
		return HttpResponse('only AJAX requests are allowed!')
	


###---------------- Ajax post to remove ONE routine from request.session['selectedRoutines']------------------###
@csrf_exempt
def remove_session(request):
	if request.is_ajax():
		mode = [{'routine': request.POST.get('routine'),}]
		rouitnePrecision = mode[0]['routine'][0]
		routineName = mode[0]['routine'][1:]
		for i, item in enumerate(request.session['selectedRoutines']):
			if item.get('routineName') == routineName and item.get('thePrecision') == rouitnePrecision:
				del request.session['selectedRoutines'][i]
				
		### important: mark the session as modified for it to save		
		request.session.modified = True

		return HttpResponse('Removed '+rouitnePrecision+routineName)		
	else:
		return HttpResponse('only AJAX requests are allowed!')

