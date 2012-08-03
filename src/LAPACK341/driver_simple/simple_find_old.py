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
        if "sv.f" in routineName:
            routines_simple_old.append(routineName)
            f_simple_old.write(routineName)
        else:
            pass


print "------------- Find 'simple driver' routines in the old version --------------"


###------------find 'simple" routines in the old version
###------------ and write them into routines/simple_old.txt
## find the routines that HAVE the keywords:
f_simple_old_single = open(parentdir+'/sortOld/routines/solve_old_single.txt')
f_simple_old_double = open(parentdir+'/sortOld/routines/solve_old_double.txt')
f_simple_old_complex = open(parentdir+'/sortOld/routines/solve_old_complex.txt')
f_simple_old_complex16 = open(parentdir+'/sortOld/routines/solve_old_complex16.txt')
f_should_be_driver = open(parentdir+'/computational_solve/routines/driver_old.txt')

f_simple_old = open('./routines/simple_old.txt', 'w')

routines_simple_old = []
start = time()


findRoutines(f_simple_old_single)
findRoutines(f_simple_old_double)
findRoutines(f_simple_old_complex)
findRoutines(f_simple_old_complex16)


#check in the computational group
print "The following simple routines should NOT be in the computational group:"
i = 0
for routineName in f_should_be_driver:
    if "sv.f" in routineName:
        i += 1
        routines_simple_old.append(routineName)
        f_simple_old.write(routineName)
        print i, ' ', routineName
    else:
        pass

f_should_be_driver.close()
    
elapsed = (time() - start)
print "There are %s simple driver routines in the old version." % len(routines_simple_old), elapsed







