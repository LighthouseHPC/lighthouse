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
        if "svx" in routineName:
            routines_expert_old.append(routineName)
            f_expert_old.write(routineName)
        else:
            pass


print "------------- Find 'expert driver' routines in the old version --------------"


###------------find 'expert" routines in the old version
###------------ and write them into routines/expert_old.txt
## find the routines that HAVE the keywords:
f_expert_old_single = open(parentdir+'/sortOld/routines/solve_old_single.txt')
f_expert_old_double = open(parentdir+'/sortOld/routines/solve_old_double.txt')
f_expert_old_complex = open(parentdir+'/sortOld/routines/solve_old_complex.txt')
f_expert_old_complex16 = open(parentdir+'/sortOld/routines/solve_old_complex16.txt')
f_should_be_driver = open(parentdir+'/computational_solve/routines/driver_old.txt')

f_expert_old = open('./routines/expert_old.txt', 'w')

routines_expert_old = []
start = time()


findRoutines(f_expert_old_single)
findRoutines(f_expert_old_double)
findRoutines(f_expert_old_complex)
findRoutines(f_expert_old_complex16)


#check in the computational group
print "The following expert routines should NOT be in the computational group:"
i = 0
for routineName in f_should_be_driver:
    if "svx" in routineName:
        i += 1
        routines_expert_old.append(routineName)
        f_expert_old.write(routineName)
        print i, ' ', routineName
    else:
        pass

f_should_be_driver.close()
    
elapsed = (time() - start)
print "There are %s expert driver routines in the old version." % len(routines_expert_old), elapsed







