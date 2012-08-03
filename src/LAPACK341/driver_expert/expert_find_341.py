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
            routines_expert_341.append(routineName)
            f_expert_341.write(routineName)
        else:
            pass

    fileName.close()

print "------------- Find 'expert driver' routines in v3.4.1 --------------"


###------------ find routines that compute the expert of a matrix in the new version
###------------ and write them into routines/expert_341.txt
## find the routines that HAVE the keywords:
f_expert_341_single = open(parentdir+'/sort341/routines/solve_341_single.txt')
f_expert_341_double = open(parentdir+'/sort341/routines/solve_341_double.txt')
f_expert_341_complex = open(parentdir+'/sort341/routines/solve_341_complex.txt')
f_expert_341_complex16 = open(parentdir+'/sort341/routines/solve_341_complex16.txt')
f_should_be_driver = open(parentdir+'/computational_solve/routines/driver_341.txt')

f_expert_341 = open('./routines/expert_341.txt', 'w')

routines_expert_341 = []
start = time()


findRoutines(f_expert_341_single)
findRoutines(f_expert_341_double)
findRoutines(f_expert_341_complex)
findRoutines(f_expert_341_complex16)

#check in the computational group
print "The following expert routines should NOT be in the computational group:"
i = 0
for routineName in f_should_be_driver:
    if "svx" in routineName:
        i += 1
        routines_expert_341.append(routineName)
        f_expert_341.write(routineName)
        print i, ' ', routineName
    else:
        pass
    
f_should_be_driver.close()
        
elapsed = (time() - start)
print "There are %s expert driver routines in the 341 version." % len(routines_expert_341), elapsed


