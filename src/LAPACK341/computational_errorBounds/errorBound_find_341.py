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
                                routines_errorBound_341.append(routineName)
                            else:
                                print "should be driver_expert: ", routineName
                        else:
                            pass

    fileName.close()

print "------------- Find 'error bound' routines in v3.4.1 --------------"


###------------ find routines that compute the errorBound of a matrix in the new version
###------------ and write them into routines/errorBound_341.txt
## find the routines that HAVE the keywords:
f_computational_341_single = open(parentdir+'/sort341/routines/computational_341_single.txt')
f_computational_341_double = open(parentdir+'/sort341/routines/computational_341_double.txt')
f_computational_341_complex = open(parentdir+'/sort341/routines/computational_341_complex.txt')
f_computational_341_complex16 = open(parentdir+'/sort341/routines/computational_341_complex16.txt')
f_errorBound_341 = open('./routines/errorBound_341.txt', 'w')
routines_errorBound_341 = []
start = time()


findRoutines(f_computational_341_single)
findRoutines(f_computational_341_double)
findRoutines(f_computational_341_complex)
findRoutines(f_computational_341_complex16)


## remove duplicates
routines_errorBound_341 = set(routines_errorBound_341)

## write to the file ./routines/errorBound_341.txt.
for routineName in routines_errorBound_341:
    f_errorBound_341.write(routineName)


elapsed = (time() - start)
print "There are %s routines in the 341 version that provides error bounds." % len(routines_errorBound_341), elapsed


