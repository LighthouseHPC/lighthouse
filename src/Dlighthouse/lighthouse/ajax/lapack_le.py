from django.utils import simplejson
from dajaxice.decorators import dajaxice_register
from dajaxice.core import dajaxice_functions
from dajax.core import Dajax
import os, glob, zipfile
from datetime import datetime
from codeGen.templates import BTOGenerator
from lighthouse.templateGen.lapack_le import generateTemplate, generateTemplate_C


dir_download = "./static/download/"
extension_dic = {'fortran': 'f90', 'cpp': 'c'} 

@dajaxice_register
def createTemplate(request, checked_list, language, time):
	dajax = Dajax()
	dajax.add_css_class("#template_output", "brush: %s;"%language)
	try:
		extension = extension_dic[language]
		file_zip = zipfile.ZipFile(dir_download+"lighthouse_%s.zip"%time, "w")
		makeFile("temp_%s.%s"%(checked_list[0].lower(), extension))
		with open(dir_download+'%s.%s'%(time, extension), 'w') as outfile:
			for item in checked_list:
				item = item.lower()
				if language == 'cpp':
					go = generateTemplate_C(item)
				else:
					go = generateTemplate(item)
				go.make_template()
				name = "./lighthouse/templateGen/%s/codeTemplates/temp_%s.%s"%(language, item, extension)
				file_zip.write(name, os.path.basename(name), zipfile.ZIP_DEFLATED)
				with open(name,"r") as infile:
					outfile.write(infile.read())
					
		## display f_output, which contains all routines
		f_output = open(dir_download+"%s.%s"%(time, extension),"r")
		dajax.assign("#template_output", 'innerHTML', f_output.read())
		dajax.script('SyntaxHighlighter.highlight()')
		f_output.close()
		
		## write README into the zip file
		file_zip.write("./lighthouse/templateGen/README", os.path.basename(dir_download+"README"), zipfile.ZIP_DEFLATED)
		
		## write makefile into the zip file
		file_zip.write(dir_download+"makefile", os.path.basename(dir_download+"makefile"), zipfile.ZIP_DEFLATED)
		file_zip.close()
		
		## remove makefile from the download directory
		os.remove(dir_download+"makefile")
		
	except:
		dajax.assign("#template_output", 'innerHTML', 'Coming soon...')		

	return dajax.json()
	



def makeFile(file_name):
	with open(dir_download+'makefile', 'w') as outfile:
		outfile.write("# This is a simple example of how to compile a program containing LAPACK routines. \n\n")
		if ".f90" in file_name:
			outfile.write("CC=gfortran\nCFLAGS=-llapack -lblas\n\n")	
		elif ".c" in file_name:
			outfile.write("CC=gcc\nCFLAGS=-llapack -lblas\n\n")
		outfile.write("lapackout: %s\n"%file_name)
		outfile.write("\t$(CC) $(CFLAGS) -o lapackout %s"%file_name)	




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
		outputls = output.replace("<", "&lt;")
		outputls = outputls.replace(">", "&gt;")
		dajax.assign("#script_output", 'innerHTML', outputls)
		dajax.script('SyntaxHighlighter.highlight()')
		f = open("%s/static/download/script/%s.c"%(defaultDir, paramProperty['kernelName']),"w")
		f.write(output)
		f.close()
	
	except Exception, e:
		print 'cannot display output because of: '
	

	os.chdir(defaultDir)

	dajax.script('dojo.byId("waitScript").style.display = "none"')
	dajax.script('waitStandby.hide()')
	#dajax.script('stopTimer()')
		     
	return dajax.json()
