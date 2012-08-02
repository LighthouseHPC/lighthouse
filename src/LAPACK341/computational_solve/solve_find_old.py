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
        if "trs.f" in routineName:
            routines_solve_old.append(routineName)
            f_solve_old.write(routineName)
        elif "rfs.f" in routineName:
            #print "for error bounds: ", routineName
            pass
        elif "sv" in routineName:
            if "svp" in routineName:
                pass
            else:
                driver_old.append(routineName)
                f_driver_old.write(routineName)
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
                    index1 = line.find("system of linear")
                    if index1 > -1:
                        routines_solve_old.append(routineName)
                        f_solve_old.write(routineName)
                    else:
                        index2 = line.find("solves the equation")
                        if index2 > -1:
                            routines_solve_old.append(routineName)
                            f_solve_old.write(routineName)
                        else:
                            index3 = line.find("system of the form")
                            if index3 > -1:
                                routines_solve_old.append(routineName)
                                f_solve_old.write(routineName)
                            else:
                                index4 = line.find("systems of equations")
                                if index4 > -1:
                                    routines_solve_old.append(routineName)
                                    f_solve_old.write(routineName)
                                else:
                                    pass

    fileName.close()

print "------------- Find 'solve' routines in the old version --------------"


###------------find 'solve" routines in the old version
###------------ and write them into routines/solve_old.txt
## find the routines that HAVE the keywords:
f_computational_old_single = open(parentdir+'/sortOld/routines/computational_old_single.txt')
f_computational_old_double = open(parentdir+'/sortOld/routines/computational_old_double.txt')
f_computational_old_complex = open(parentdir+'/sortOld/routines/computational_old_complex.txt')
f_computational_old_complex16 = open(parentdir+'/sortOld/routines/computational_old_complex16.txt')
f_solve_old = open('./routines/solve_old.txt', 'w')
#write the driver routines in driver_old.txt.
f_driver_old = open('./routines/driver_old.txt', 'w')
routines_solve_old = []
driver_old = []
start = time()


findRoutines(f_computational_old_single)
findRoutines(f_computational_old_double)
findRoutines(f_computational_old_complex)
findRoutines(f_computational_old_complex16)

f_driver_old.close()
f_solve_old.close()
elapsed = (time() - start)
print "%s routines should be in the Driver category, not Computational." % len(driver_old)
print "There are %s routines in the old version that provides solve." % len(routines_solve_old), elapsed







