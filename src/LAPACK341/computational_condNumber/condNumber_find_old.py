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
            routines_condNumber_old.append(routineName)
            f_condNumber_old.write(routineName)
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
                            routines_condNumber_old.append(routineName)
                        else:
                            pass

    fileName.close()

print "------------- Find 'condition number' routines in the old version --------------"


###------------find 'condNumber" routines in the old version
###------------ and write them into routines/condNumber_old.txt
## find the routines that HAVE the keywords:
f_computational_old_single = open(parentdir+'/sortOld/routines/computational_old_single.txt')
f_computational_old_double = open(parentdir+'/sortOld/routines/computational_old_double.txt')
f_computational_old_complex = open(parentdir+'/sortOld/routines/computational_old_complex.txt')
f_computational_old_complex16 = open(parentdir+'/sortOld/routines/computational_old_complex16.txt')
f_condNumber_old = open('./routines/condNumber_old.txt', 'w')
routines_condNumber_old = []
start = time()


findRoutines(f_computational_old_single)
findRoutines(f_computational_old_double)
findRoutines(f_computational_old_complex)
findRoutines(f_computational_old_complex16)


## eliminate duplicates
routines_condNumber_old = list(set(routines_condNumber_old))
for routineName in routines_condNumber_old:
    f_condNumber_old.write(routineName)
    
    
elapsed = (time() - start)
print "There are %s routines in the old version that provides condition number." % len(routines_condNumber_old), elapsed







