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
        if 'trf.f' in routineName:
            routines_factor_old.append(routineName)
            f_factor_old.write(routineName)
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
                                    routines_factor_MN_old.append(routineName)
                                    f_factor_MN_old.write(routineName)
                                else:            
                                    index4 = line.find("N-by-M matrix")
                                    if index4 > -1:
                                        routines_factor_NM_old.append(routineName)
                                        f_factor_NM_old.write(routineName)
                                    else:
                                        if "stf" not in routineName:
                                            routines_factor_old.append(routineName)
                                            f_factor_old.write(routineName)
                                        else:
                                            print "split Cholesky factorization: ", routineName
    fileName.close()

print "------------- Find 'factor' routines in the old version --------------"


###------------find 'factor" routines in the old version
###------------ and write them into routines/factor_old.txt
## find the routines that HAVE the keywords:
f_computational_old_single = open(parentdir+'/sortOld/routines/computational_old_single.txt')
f_computational_old_double = open(parentdir+'/sortOld/routines/computational_old_double.txt')
f_computational_old_complex = open(parentdir+'/sortOld/routines/computational_old_complex.txt')
f_computational_old_complex16 = open(parentdir+'/sortOld/routines/computational_old_complex16.txt')

f_factor_old = open('./routines/factor_old.txt', 'w')
routines_factor_old = []
f_factor_MN_old = open('./routines/factor_M_by_N_old.txt', 'w')
routines_factor_MN_old = []
f_factor_NM_old = open('./routines/factor_N_by_M_old.txt', 'w')
routines_factor_NM_old = []
start = time()


findRoutines(f_computational_old_single)
findRoutines(f_computational_old_double)
findRoutines(f_computational_old_complex)
findRoutines(f_computational_old_complex16)

    
elapsed = (time() - start)
print "There are %s routines in the old version that provides factor." % len(routines_factor_old), elapsed







