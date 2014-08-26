from django.utils import simplejson
from dajaxice.decorators import dajaxice_register
from dajaxice.core import dajaxice_functions
from dajax.core import Dajax
import os, glob, zipfile
from datetime import datetime
from lighthouse.codeGen.templates import BTOGenerator


dir_download = "./static/download/"


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
		
	if inoutArray:
		f.write('inout\n')
		f.write('  ')
		for item in inoutArray[:-1]:
			f.write('%s, '% (item))
		f.write('%s \n'%inoutArray[-1])
		
	if outArray:
		f.write('out\n')
		f.write('  ')
		for item in outArray[:-1]:
			f.write('%s, '% (item))
		f.write('%s \n'%outArray[-1])

	f.write('{\n')
	f.write('   %s\n'%paramProperty['equation'])
	f.write('}')
	f.close()

	btoArgs = paramProperty['btoArgs'];

	try:
		output = bto.submitToBTO('%s.m'%paramProperty['kernelName'], btoArgs)

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


