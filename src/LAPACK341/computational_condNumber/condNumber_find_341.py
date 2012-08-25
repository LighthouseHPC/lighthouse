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
        f = urllib.urlopen(url)
        if "sna" in routineName or "equ" in routineName or "svx" in routineName or "sen" in routineName or "grw.f" in routineName:
            pass
        elif "con.f" in routineName:        
            routines_condNumber_341.append(routineName)
        else:
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
                        index1 = line.find("condition number")
                        if index1 > -1:
                            routines_condNumber_341.append(routineName)
                        else:
                            pass


    fileName.close()

print "------------- Find 'condition number' routines in v3.4.1 --------------"


###------------ find routines that compute the condition number of a matrix in the new version
###------------ and write them into routines/condNumber_341.txt
## find the routines that HAVE the keywords:
f_computational_341_single = open(parentdir+'/sort341/routines/computational_341_single.txt')
f_computational_341_double = open(parentdir+'/sort341/routines/computational_341_double.txt')
f_computational_341_complex = open(parentdir+'/sort341/routines/computational_341_complex.txt')
f_computational_341_complex16 = open(parentdir+'/sort341/routines/computational_341_complex16.txt')
f_condNumber_341 = open('./routines/condNumber_341.txt', 'w')
routines_condNumber_341 = []
start = time()


findRoutines(f_computational_341_single)
findRoutines(f_computational_341_double)
findRoutines(f_computational_341_complex)
findRoutines(f_computational_341_complex16)


## eliminate duplicates
routines_condNumber_341 = set(routines_condNumber_341)

## write condNumber_341.txt
for routineName in routines_condNumber_341:
    f_condNumber_341.write(routineName)
    
elapsed = (time() - start)
print "There are %s routines in the 341 version that provides condition number." % len(routines_condNumber_341), elapsed


