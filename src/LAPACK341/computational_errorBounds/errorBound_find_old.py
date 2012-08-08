import urllib, shutil, csv
from time import time
import os
parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
os.sys.path.insert(0,parentdir) 
import summary.summary


def findRoutines(fileName):
    for ln in fileName:
        url = ln.split(";")[3]
        routineName = url.split("/")[-1]
        #print routineName
        #print url
        if "grw.f" in routineName or "amv.f" in routineName:
            pass
        else:
            f = urllib.urlopen(url)
            flag = 1
            for line in f:
                line = line[3:]
                #print line
                if line.startswith("Arguments"):
                    break
                else:
                    if line.startswith("\par Purpose:"):
                        flag = 0
                    if line.startswith("Arguments"):
                        flag = 1
                    if not flag:
                        index1 = line.find("error bounds")
                        if index1 > -1:
                            if "svx" not in routineName:
                                routines_errorBound_old.append(routineName)
                                f_errorBound_old.write(routineName)
                            else:
                                print "should be driver_expert: ", routineName
                        else:
                            pass

    fileName.close()

print "------------- Find 'error bounds' routines in the old version --------------"


###------------find 'error bounds" routines in the old version
###------------ and write them into routines/errorBound_old.txt
## find the routines that HAVE the keywords:
f_computational_old_single = open(parentdir+'/sortOld/routines/computational_old_single.txt')
f_computational_old_double = open(parentdir+'/sortOld/routines/computational_old_double.txt')
f_computational_old_complex = open(parentdir+'/sortOld/routines/computational_old_complex.txt')
f_computational_old_complex16 = open(parentdir+'/sortOld/routines/computational_old_complex16.txt')
f_errorBound_old = open('./routines/errorBound_old.txt', 'w')
routines_errorBound_old = []
start = time()


findRoutines(f_computational_old_single)
findRoutines(f_computational_old_double)
findRoutines(f_computational_old_complex)
findRoutines(f_computational_old_complex16)

    
elapsed = (time() - start)
print "There are %s routines in the old version that provides error bounds." % len(routines_errorBound_old), elapsed







