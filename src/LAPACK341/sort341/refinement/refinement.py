import urllib, shutil, csv
from time import time
import os
parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
os.sys.path.insert(0,parentdir) 

print parentdir

def findRoutines(fileName):
    for ln in fileName:
        url = ln.split(";")[3]
        routineName = url.split("/")[-1]
        #print routineName
        #print url
        f = urllib.urlopen(url)
        flag = 1
        for line in f:
            line = line[3:]
            #print line
            if line.startswith("Authors:"):
                break
            else:
                if line.startswith("\par Purpose:"):
                    flag = 0
                if line.startswith("Authors:"):
                    flag = 1
                if not flag:
                    index1 = line.find("refinement")
                    if index1 > -1:
                        routines_refinement_341.append(routineName)
                    else:
                        pass


    fileName.close()

print "------------- Find 'refinement' routines in v3.4.1 --------------"


###------------ find routines that compute the refinement of a matrix in the new version
###------------ and write them into routines/refinement_341.txt
## find the routines that HAVE the keywords:
f_solve_341_single = open(parentdir+'/routines/solve_341_single.txt')
f_solve_341_double = open(parentdir+'/routines/solve_341_double.txt')
f_solve_341_complex = open(parentdir+'/routines/solve_341_complex.txt')
f_solve_341_complex16 = open(parentdir+'/routines/solve_341_complex16.txt')

f_computational_341_single = open(parentdir+'/routines/computational_341_single.txt')
f_computational_341_double = open(parentdir+'/routines/computational_341_double.txt')
f_computational_341_complex = open(parentdir+'/routines/computational_341_complex.txt')
f_computational_341_complex16 = open(parentdir+'/routines/computational_341_complex16.txt')

f_refinement_341 = open('./routines/refinement_341.txt', 'w')
f_not_refinement_341 = open('./routines/not_refinement_341.txt', 'w')
routines_refinement_341 = []
routines_not_refinement_341 = []
start = time()


findRoutines(f_solve_341_single)
findRoutines(f_solve_341_double)
findRoutines(f_solve_341_complex)
findRoutines(f_solve_341_complex16)
findRoutines(f_computational_341_single)
findRoutines(f_computational_341_double)
findRoutines(f_computational_341_complex)
findRoutines(f_computational_341_complex16)


## find the routines that do not do iterative refinement
for routineName in routines_refinement_341:
    f = urllib.urlopen('http://www.netlib.org/lapack/lapack_routine/'+routineName)
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
                index2 = line.find("does not do iterative")
                if index2 > -1:
                    routines_not_refinement_341.append(routineName)
                    f_not_refinement_341.write(routineName)
                else:
                    pass
                    

## substarct the routines that do not do iterative refinement.
routines_refinement_341 = list(set(routines_refinement_341) - set(routines_not_refinement_341))

for routineName in routines_refinement_341:
    f_refinement_341.write(routineName)

elapsed = (time() - start)
print "%s routines that do not do iterative refinement." % len(routines_not_refinement_341)
print "There are %s routines in the 341 version that provides refinement." % len(routines_refinement_341), elapsed

