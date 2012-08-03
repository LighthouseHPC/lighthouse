import urllib, shutil, csv
from time import time
import os
parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
os.sys.path.insert(0,parentdir) 
import summary.summary

print "------------- Find the routines that solve a system of linear equations in the old version --------------"


def findRoutines(fileName):
    for ln in fileName:
        url = ln.split(";")[3]
        routineName = url.split("/")[-1]
        print routineName
        #print url
        f = urllib.urlopen(url)
        if 'rfs.f' in routineName:
            pass
        else:
            flag = 1
            for line in f:
                if "================================" in line:
                    break
                else:
                    line = line[3:]
                    if "\par Purpose:" in line:
                        flag = 0
                    if line.startswith("Arguments:"):
                        flag = 1
                    if not flag:
                        index1 = line.find("system of linear")
                        if index1 > -1:
                            routines_linearSolve_old.append(routineName)
                            f_linearSolve_old.write(routineName+'\n')
                        else:
                            index2 = line.find("solves the equation")
                            if index2 > -1:
                                routines_linearSolve_old.append(routineName)
                                f_linearSolve_old.write(routineName+'\n')
                            else:
                                index3 = line.find("system of the form")
                                if index3 > -1:
                                    routines_linearSolve_old.append(routineName)
                                    f_linearSolve_old.write(routineName+'\n')
                                else:
                                    index4 = line.find("systems of equations")
                                    if index4 > -1:
                                        routines_linearSolve_old.append(routineName)
                                        f_linearSolve_old.write(routineName+'\n')
                                    else:
                                        pass

    fileName.close()

print "------------- Find 'linearSolve' routines in the old version --------------"


###------------find 'linearSolve" routines in the old version
###------------ and write them into routines/linearSolve_old.txt
## find the routines that HAVE the keywords:
f_computational_old_single = open(parentdir+'/sortOld/routines/computational_old_single.txt')
f_computational_old_double = open(parentdir+'/sortOld/routines/computational_old_double.txt')
f_computational_old_complex = open(parentdir+'/sortOld/routines/computational_old_complex.txt')
f_computational_old_complex16 = open(parentdir+'/sortOld/routines/computational_old_complex16.txt')
f_linearSolve_old = open('./routines/linearSolve_old.txt', 'w')
routines_linearSolve_old = []
start = time()


findRoutines(f_computational_old_single)
findRoutines(f_computational_old_double)
findRoutines(f_computational_old_complex)
findRoutines(f_computational_old_complex16)

    
elapsed = (time() - start)
print "There are %s routines in the old version that provides linearSolve." % len(routines_linearSolve_old), elapsed
