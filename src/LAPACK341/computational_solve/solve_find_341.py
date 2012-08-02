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
            routines_solve_341.append(routineName)
            f_solve_341.write(routineName)
        elif "rfs.f" in routineName:
            print "should be for error bounds: ", routineName
            #pass
        elif "rfsx.f" in routineName:
            print "should be for error bounds: ", routineName
            #pass
        elif "sv" in routineName:
            if "svp" in routineName:
                pass
            elif "jsv" in routineName:
                print "shoule be for SVD: ", routineName
            elif "svj" in routineName:
                pass
            else:
                driver_341.append(routineName)
                f_driver_341.write(routineName)
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
                        routines_solve_341.append(routineName)
                        f_solve_341.write(routineName)
                    else:
                        index2 = line.find("solves the equation")
                        if index2 > -1:
                            routines_solve_341.append(routineName)
                            f_solve_341.write(routineName)
                        else:
                            index3 = line.find("system of the form")
                            if index3 > -1:
                                routines_solve_341.append(routineName)
                                f_solve_341.write(routineName)
                            else:
                                index4 = line.find("systems of equations")
                                if index4 > -1:
                                    routines_solve_341.append(routineName)
                                    f_solve_341.write(routineName)
                                else:
                                    pass

    fileName.close()

print "------------- Find 'solve' routines in v3.4.1 --------------"


###------------ find routines that compute the solve of a matrix in the new version
###------------ and write them into routines/solve_341.txt
## find the routines that HAVE the keywords:
f_computational_341_single = open(parentdir+'/sort341/routines/computational_341_single.txt')
f_computational_341_double = open(parentdir+'/sort341/routines/computational_341_double.txt')
f_computational_341_complex = open(parentdir+'/sort341/routines/computational_341_complex.txt')
f_computational_341_complex16 = open(parentdir+'/sort341/routines/computational_341_complex16.txt')
f_solve_341 = open('./routines/solve_341.txt', 'w')
routines_solve_341 = []
#write the driver routines in driver_341.txt.
f_driver_341 = open('./routines/driver_341.txt', 'w')
driver_341 = []
start = time()

start = time()


findRoutines(f_computational_341_single)
findRoutines(f_computational_341_double)
findRoutines(f_computational_341_complex)
findRoutines(f_computational_341_complex16)

f_driver_341.close()
f_solve_341.close()
elapsed = (time() - start)
print "%s routines should be in the Driver category, not Computational." % len(driver_341)
print "There are %s routines in the 341 version that provides solve." % len(routines_solve_341), elapsed


