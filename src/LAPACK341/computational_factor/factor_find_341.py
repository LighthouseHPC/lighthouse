import urllib, shutil, csv
from time import time
import os
parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
os.sys.path.insert(0,parentdir) 
import summary.summary

def findRoutines(fileName):
    i = len(routines_factor_MN)
    j = len(routines_factor_NM)
    k = len(routines_factor)
    for ln in fileName:
        url = ln.split(";")[3]
        routineName = url.split("/")[-1]
        #print url
        f = urllib.urlopen(url)
        if 'trf.f' in routineName:
            routines_factor.append(routineName)
            wr3.write(routineName)
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
                        index1 = line.find("factorization of a")
                        if index1 == -1:
                            pass
                        else:
                            index2 = line.find("computes")
                            if index2 == -1:
                                pass
                            else:
                                index3 = line.find("M-by-N matrix")
                                if index3 > -1:
                                    i += 1
                                    routines_factor_MN.append(routineName)
                                    wr1.write(routineName)
                                else:            
                                    index4 = line.find("N-by-M matrix")
                                    if index4 > -1:
                                        j += 1
                                        routines_factor_NM.append(routineName)
                                        wr2.write(routineName)
                                    else:
                                        if "stf" not in routineName:
                                            k += 1
                                            routines_factor.append(routineName)
                                            wr3.write(routineName)
                                        else:
                                            print "split Cholesky factorization: ", routineName
    fileName.close()

print "------------- Find 'factor' routines in v3.4.1 --------------"


###------------ find routines that compute the factor of a matrix in the new version
###------------ and write them into routines/factor_341.txt
## find the routines that HAVE the keywords:
f_computational_341_single = open(parentdir+'/sort341/routines/computational_341_single.txt')
f_computational_341_double = open(parentdir+'/sort341/routines/computational_341_double.txt')
f_computational_341_complex = open(parentdir+'/sort341/routines/computational_341_complex.txt')
f_computational_341_complex16 = open(parentdir+'/sort341/routines/computational_341_complex16.txt')
wr1 = open('routines/factor_M_by_N_341.txt', 'w')
routines_factor_MN = []
wr2 = open('routines/factor_N_by_M_341.txt', 'w')
routines_factor_NM = []
wr3 = open('routines/factor_341.txt', 'w')
routines_factor = []
start = time()


findRoutines(f_computational_341_single)
findRoutines(f_computational_341_double)
findRoutines(f_computational_341_complex)
findRoutines(f_computational_341_complex16)

    
elapsed = (time() - start)
print "There are %s routines in the 341 version that provides factor." % len(routines_factor), elapsed


