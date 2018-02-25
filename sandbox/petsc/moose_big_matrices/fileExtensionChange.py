import os,sys
import subprocess
folder = '/Users/kanikas/Documents/github/Lighthouse_recent/lighthouse/sandbox/petsc/moose_big_matrices/moose-timing-logs'
for filename in os.listdir(folder):
	infilename = os.path.join(folder,filename)
	if not os.path.isfile(infilename): continue
	oldbase = os.path.splitext(filename)
	newname = infilename.replace('.p24.log', '.log')
	output = os.rename(infilename, newname)
	print "Replaced .p24.log with .log"
#subprocess.call(['./changeExtension.sh'],shell=True)

