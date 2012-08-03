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
            routines_simple_341.append(routineName)
            f_simple_341.write(routineName)
        else:
            pass

    fileName.close()

print "------------- Find 'simple driver' routines in v3.4.1 --------------"


###------------ find routines that compute the simple of a matrix in the new version
###------------ and write them into routines/simple_341.txt
## find the routines that HAVE the keywords:
f_simple_341_single = open(parentdir+'/sort341/routines/solve_341_single.txt')
f_simple_341_double = open(parentdir+'/sort341/routines/solve_341_double.txt')
f_simple_341_complex = open(parentdir+'/sort341/routines/solve_341_complex.txt')
f_simple_341_complex16 = open(parentdir+'/sort341/routines/solve_341_complex16.txt')
f_should_be_driver = open(parentdir+'/computational_solve/routines/driver_341.txt')

f_simple_341 = open('./routines/simple_341.txt', 'w')

routines_simple_341 = []
start = time()


findRoutines(f_simple_341_single)
findRoutines(f_simple_341_double)
findRoutines(f_simple_341_complex)
findRoutines(f_simple_341_complex16)

#check in the computational group
print "The following simple routines should NOT be in the computational group:"
i = 0
for routineName in f_should_be_driver:
    if "sv.f" in routineName:
        i += 1
        routines_simple_341.append(routineName)
        f_simple_341.write(routineName)
        print i, ' ', routineName
    else:
        pass
    
f_should_be_driver.close()
        
elapsed = (time() - start)
print "There are %s simple driver routines in the 341 version." % len(routines_simple_341), elapsed


