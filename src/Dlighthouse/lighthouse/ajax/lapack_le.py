from django.utils import simplejson
from dajaxice.decorators import dajaxice_register
from dajaxice.core import dajaxice_functions
from dajax.core import Dajax
import os, glob
from datetime import datetime
from codeGen.templates import BTOGenerator
from lighthouse.templateGen.lapack_le import generateTemplate, generateTemplate_C


@dajaxice_register
def createTemplate(request, checked_list, language):
	dajax = Dajax()
	dajax.add_css_class("#template_output", "brush: %s;"%language)
	time = 	datetime.now().strftime('%Y%m%d%H-%M-%S')
	if language == 'fortran':
		with open('./lighthouse/templateGen/output/%s.f90'%time, 'w') as outfile:
			for item in checked_list:
				item = item.lower()
				go = generateTemplate(item)
				go.make_template()
				with open("./lighthouse/templateGen/fortran/codeTemplates/temp_%s.f90"%item,"r") as infile:
					outfile.write(infile.read())
		f_output = open("./lighthouse/templateGen/output/%s.f90"%time,"r")
	elif language == 'cpp':
		with open('./lighthouse/templateGen/output/%s.c'%time, 'w') as outfile:
			for item in checked_list:
				item = item.lower()
				go = generateTemplate_C(item)
				go.make_template()
				with open("./lighthouse/templateGen/C/codeTemplates/temp_%s.c"%item,"r") as infile:
					outfile.write(infile.read())
		f_output = open("./lighthouse/templateGen/output/%s.c"%time,"r")
		
	dajax.assign("#template_output", 'innerHTML', f_output.read())
	dajax.script('SyntaxHighlighter.highlight()')
	f_output.close()

	return dajax.json()
	


#@dajaxice_register
#def removeTemplateFile(request):
#	dajax = Dajax()
#	fileName = generatedCodeTemplate_dir + request.session.session_key;
#	if os.path.isfile(fileName + '.c'):
#		os.remove(fileName + '.c')
#	elif os.path.isfile(fileName + '.f'):
#		os.remove(fileName + '.f')
#
#	return dajax.json()



@dajaxice_register
def make_mfile(request, paramProperty):
	dajax = Dajax()
	
	### set defaultDir = /homes/salin/Lighthouse/Dlighthouse/
	defaultDir = os.getcwd()

	### call codeGen.templates.BTOGenerator
	bto = BTOGenerator()
	
	### create /private/tmp/lighthouse_temp and go there
	bto.generateTmpDir()
	
	### create and write .m file in the current work dir: /private/tmp/lighthouse_temp
	inArray = []
	outArray = []
	inoutArray = []
	### filename = [kernelName].m
	f=open('%s.m'%paramProperty['kernelName'], 'w')
	f.write('%s\n'%paramProperty['kernelName'])
	for item in paramProperty:
		if paramProperty[item][0] == 'in':
			inArray.append(str(item+': '+paramProperty[item][1]))
		if paramProperty[item][0] == 'out':
			outArray.append(str(item+': '+paramProperty[item][1]))
		if paramProperty[item][0] == 'inout':
			inoutArray.append(str(item+': '+paramProperty[item][1]))
			
	if inArray:
		f.write('in\n')
		f.write('  ')
		for item in inArray[:-1]:
			f.write('%s, '% (item))
		f.write('%s \n'%inArray[-1])
		
	if outArray:
		f.write('out\n')
		f.write('  ')
		for item in outArray[:-1]:
			f.write('%s, '% (item))
		f.write('%s \n'%outArray[-1])
		
	if inoutArray:
		f.write('inout\n')
		f.write('  ')
		for item in inoutArray[:-1]:
			f.write('%s, '% (item))
		f.write('%s \n'%inoutArray[-1])
		
	f.write('{\n')
	f.write('   %s\n'%paramProperty['equation'])
	f.write('}')
	f.close()
	
	try:
		output = bto.submitToBTO('%s.m'%paramProperty['kernelName'])

	except Exception, e:
		print 'submitToBTO Exception caught:', str(e)
		print 'bto.submitToBTO(','%s.m'%paramProperty['kernelName'],')'
		
	try: 
		dajax.assign("#script_output", 'innerHTML', output)
	
	except Exception, e:
		print 'cannot display output because of: '
	

	os.chdir(defaultDir)

	return dajax.json()
