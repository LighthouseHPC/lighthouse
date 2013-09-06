from django.utils import simplejson
from dajaxice.decorators import dajaxice_register
from dajax.core import Dajax
from dajaxice.core import dajaxice_functions
import os, glob
from codeGen.templates import BTOGenerator
from lighthouse.templateGen.lapack_le import generateTemplate, generateTemplate_C

generatedCodeTemplate_dir = './lighthouse/libraries/lapack_le/generatedCodeTemplate/'
txtpath = 'lighthouse/libraries/lapack_le/databaseMng/RoutineInfo/RoutineTxt'


@dajaxice_register
def createTemplate_FORTRAN(request, checked_list):
        dajax = Dajax()
	file_list = []
	for files in glob.glob("./lighthouse/templateGen/fortran/codeTemplates/*.f90"):
	    file_list.append(files)
        for item in checked_list:
		item = item.lower()
		file_name = './lighthouse/templateGen/fortran/codeTemplates/temp_'+item+'.f90'
		if file_name not in file_list:
			go = generateTemplate(item)
			go.make_template()
		f_output = open("./lighthouse/templateGen/fortran/codeTemplates/temp_%s.f90"%item,"r")
		text = f_output.read()
		dajax.assign("#template_output", 'innerHTML', text)
                f_output.close()

        return dajax.json()



@dajaxice_register
def createTemplate_C(request, checked_list):
        dajax = Dajax()
	file_list = []
	for files in glob.glob("./lighthouse/templateGen/C/codeTemplates/*.c"):
	    file_list.append(files)
        for item in checked_list:
		item = item.lower()
		file_name = './lighthouse/templateGen/C/codeTemplates/temp_'+item+'.c'
		if file_name not in file_list:
			go = generateTemplate_C(item)
			go.make_template()
		f_output = open("./lighthouse/templateGen/C/codeTemplates/temp_%s.c"%item,"r")
		text = f_output.read()
		dajax.assign("#template_output", 'innerHTML', text)
                f_output.close()

        return dajax.json()
	




@dajaxice_register
def removeTemplateFile(request):
	dajax = Dajax()

	fileName = generatedCodeTemplate_dir + request.session.session_key;
	
	if os.path.isfile(fileName + '.c'):
		os.remove(fileName + '.c')
	elif os.path.isfile(fileName + '.f'):
		os.remove(fileName + '.f')

	return dajax.json()



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
