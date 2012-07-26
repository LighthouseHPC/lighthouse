import os, urllib, shutil, csv
from summary import *


print "------------- Find the routines that solve a system of linear equations in the old version --------------"

old_list_url = Old_List_URL()


###------------find 'factor"  routines in the old version
###------------ and write them into routines/factor_old.txt, routines/factor_M_by_N_old.txt, routines/factor_N_by_M_old.txt
## find the routines that HAVE the keywords:
f_factor_old = open('routines/factor_old.txt', 'w')
routines_factor_old = []
#f_factor_MN_old = open('routines/factor_M_by_N_old.txt', 'w')
#routines_factor_MN_old = []
#f_factor_NM_old = open('routines/factor_N_by_M_old.txt', 'w')
#routines_factor_NM_old = []

for url in old_list_url:
    f_info = urllib.urlopen(url)
    routineName = url.split("/")[-1]
    flag = 1
    for line in f_info:
        if "================================" in line:
            break
        else:
            line = line[3:]
            if line.startswith("Purpose"):
                flag = 0
            if line.startswith("Arguments"):
                flag = 1
            if not flag:
                index1 = line.find("computes")
                index2 = line.find("factorization of a")
                #index3 = line.find("M-by-N matrix")
                #index4 = line.find("N-by-M matrix")
                #if index1>-1 and index2>-1 and index3>-1:
                #    routines_factor_MN_old.append(routineName)
                #    f_factor_MN_old.write(routineName+'\n')                                        
                #if index1>-1 and index2>-1 and index4>-1:
                #    routines_factor_NM_old.append(routineName)
                #    f_factor_NM_old.write(routineName+'\n')
                if index1>-1 and index2>-1:
                    routines_factor_old.append(routineName)
                    f_factor_old.write(routineName+'\n')
            else:
                pass

    f_info.close()

print "There are %s routines in the old version that factor a matrix." % len(routines_factor_old)







