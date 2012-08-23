import urllib, shutil, csv
from time import time
import os
parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
os.sys.path.insert(0,parentdir)


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
                    index1 = line.find("pivot growth factor")
                    if index1 > -1:
                        routines_pivot_growth_factor_341.append(routineName)
                        f_pivot_growth_factor_341.write(routineName)
                    else:
                        pass

    fileName.close()

print "------------- Find 'pivot growth factor' routines in v3.4.1 --------------"


###------------ find routines that compute the pivot growth factor of a matrix in the new version
###------------ and write them into routines/pivot_growth_factor_341.txt
## find the routines that HAVE the keywords:
f_solve_341_single = open(parentdir+'/routines/solve_341_single.txt')
f_solve_341_double = open(parentdir+'/routines/solve_341_double.txt')
f_solve_341_complex = open(parentdir+'/routines/solve_341_complex.txt')
f_solve_341_complex16 = open(parentdir+'/routines/solve_341_complex16.txt')

f_computational_341_single = open(parentdir+'/routines/computational_341_single.txt')
f_computational_341_double = open(parentdir+'/routines/computational_341_double.txt')
f_computational_341_complex = open(parentdir+'/routines/computational_341_complex.txt')
f_computational_341_complex16 = open(parentdir+'/routines/computational_341_complex16.txt')

f_pivot_growth_factor_341 = open('./routines/pivot_growth_factor_341.txt', 'w')
routines_pivot_growth_factor_341 = []
start = time()


findRoutines(f_solve_341_single)
findRoutines(f_solve_341_double)
findRoutines(f_solve_341_complex)
findRoutines(f_solve_341_complex16)
findRoutines(f_computational_341_single)
findRoutines(f_computational_341_double)
findRoutines(f_computational_341_complex)
findRoutines(f_computational_341_complex16)
    
elapsed = (time() - start)
print "There are %s routines in the 341 version that provides pivot growth factor." % len(routines_pivot_growth_factor_341), elapsed

