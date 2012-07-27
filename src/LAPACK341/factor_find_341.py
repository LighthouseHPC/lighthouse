import os, urllib, shutil, csv
from time import time
from summary import * 

print "------------- Find the routines that factors a matrix in v3.4.1 --------------"


###----------- get new_list

new_list = New_List()
#print len(new_list)


###------------ find routines that compute the factorization of a matrix in the new version
###------------ and write them into routines/factor_341.txt
wr1 = csv.writer(open('routines/factor_M_by_N_341.txt', 'w'), delimiter=';')
routines_factor_MN = []
wr2 = csv.writer(open('routines/factor_N_by_M_341.txt', 'w'), delimiter=';')
routines_factor_NM = []
wr3 = csv.writer(open('routines/factor_341.txt', 'w'), delimiter=';')
routines_factor = []
i=0
j=0
k=0
start = time()
for routineName in new_list:
    f = urllib.urlopen("http://www.netlib.org/lapack/lapack_routine/"+routineName)
    text = f.read()
    text = text.split("\n")
    flag = 1
    for line in text:
        if "================================" in line:
            break
        else:
            line = line[3:]
            if line.startswith("\par Purpose:"):
                flag = 0
            if line.startswith("Arguments:"):
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
                            wr1.writerow([i, routineName[0], routineName[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+routineName])
                        else:            
                            index4 = line.find("N-by-M matrix")
                            if index4 > -1:
                                j += 1
                                routines_factor_NM.append(routineName)
                                wr2.writerow([j, routineName[0], routineName[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+routineName])
                            else:
                                k += 1
                                routines_factor.append(routineName)
                                wr3.writerow([k, routineName[0], routineName[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+routineName])
            else:
                pass
f.close()

elapsed = time() - start
print "There are %s routines that compute the factorization of a matrix in v3.4.1." % len(routines_factor), elapsed 


