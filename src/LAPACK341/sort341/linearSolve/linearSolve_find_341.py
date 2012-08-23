import urllib, shutil, csv
from time import time
import os
parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
os.sys.path.insert(0,parentdir) 
import summary.summary

print "------------- Find the routines that solve a system of linear equations in the 341 version --------------"


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
                            routines_linearSolve_341.append(routineName)
                            f_linearSolve_341.write(routineName+'\n')
                        else:
                            index2 = line.find("solves the equation")
                            if index2 > -1:
                                routines_linearSolve_341.append(routineName)
                                f_linearSolve_341.write(routineName+'\n')
                            else:
                                index3 = line.find("system of the form")
                                if index3 > -1:
                                    routines_linearSolve_341.append(routineName)
                                    f_linearSolve_341.write(routineName+'\n')
                                else:
                                    index4 = line.find("systems of equations")
                                    if index4 > -1:
                                        routines_linearSolve_341.append(routineName)
                                        f_linearSolve_341.write(routineName+'\n')
                                    else:
                                        pass

    fileName.close()

print "------------- Find 'linearSolve' routines in the 341 version --------------"


###------------find 'linearSolve" routines in the 341 version
###------------ and write them into routines/linearSolve_341.txt
## find the routines that HAVE the keywords:
f_computational_341_single = open(parentdir+'/sort341/routines/computational_341_single.txt')
f_computational_341_double = open(parentdir+'/sort341/routines/computational_341_double.txt')
f_computational_341_complex = open(parentdir+'/sort341/routines/computational_341_complex.txt')
f_computational_341_complex16 = open(parentdir+'/sort341/routines/computational_341_complex16.txt')
f_linearSolve_341 = open('./routines/linearSolve_341.txt', 'w')
routines_linearSolve_341 = []
start = time()


findRoutines(f_computational_341_single)
findRoutines(f_computational_341_double)
findRoutines(f_computational_341_complex)
findRoutines(f_computational_341_complex16)

    
elapsed = (time() - start)
print "There are %s routines in the 341 version that provides linearSolve." % len(routines_linearSolve_341), elapsed
