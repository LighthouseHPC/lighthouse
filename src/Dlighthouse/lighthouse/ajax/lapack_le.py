from django.utils import simplejson
from dajaxice.decorators import dajaxice_register
from dajaxice.core import dajaxice_functions
from dajax.core import Dajax
import os, glob, zipfile
from datetime import datetime
from lighthouse.templateGen.lapack_le.lapack_le import generateTemplate, generateTemplate_C


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
				name = "./lighthouse/templateGen/lapack_le/%s/codeTemplates/temp_%s.%s"%(language, item, extension)
				file_zip.write(name, os.path.basename(name), zipfile.ZIP_DEFLATED)
				with open(name,"r") as infile:
					outfile.write(infile.read())
					
		## display f_output, which contains all routines
		f_output = open(dir_download+"%s.%s"%(time, extension),"r")
		dajax.assign("#template_output", 'innerHTML', f_output.read())
		dajax.script('SyntaxHighlighter.highlight()')
		f_output.close()
		
		## write README into the zip file
		file_zip.write("./lighthouse/templateGen/lapack_le/README", os.path.basename(dir_download+"README"), zipfile.ZIP_DEFLATED)
		
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
		outfile.write("\t$(CC) %s -o lapackout $(CFLAGS)"%file_name)	

