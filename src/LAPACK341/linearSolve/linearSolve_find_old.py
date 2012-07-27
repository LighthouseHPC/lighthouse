import os, urllib, shutil, csv
from time import time
from summary import *


print "------------- Find the routines that solve a system of linear equations in the old version --------------"

old_list_url = Old_List_URL()


###------------find 'Linear Solve' and "solves the equation" routines in the old version
###------------ and write them into routines/linearSolve_old.txt
## find the routines that HAVE the keywords:
f_linearSolve_old = open('routines/linearSolve_old.txt', 'w')
routines_solve_old = []
start = time()
for url in old_list_url:
    f_info = urllib.urlopen(url)
    routineName = url.split("/")[-1]
    if 'rfs.f' in routineName:
        pass
    else:
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
                    index1 = line.find("system of linear")
                    if index1 > -1:
                        routines_solve_old.append(routineName)
                        f_linearSolve_old.write(routineName+'\n')
                    else:
                        index2 = line.find("solves the equation")
                        if index2 > -1:
                            routines_solve_old.append(routineName)
                            f_linearSolve_old.write(routineName+'\n')
                        else:
                            index3 = line.find("system of the form")
                            if index3 > -1:
                                routines_solve_old.append(routineName)
                                f_linearSolve_old.write(routineName+'\n')
                            else:
                                index4 = line.find("systems of equations")
                                if index4 > -1:
                                    routines_solve_old.append(routineName)
                                    f_linearSolve_old.write(routineName+'\n')
                                else:
                                    pass
    f_info.close()
    
routines_solve_old = [x for x in routines_solve_old if not routines_solve_old.count(x) > 1]
elapsed = (time() - start)
print "There are %s routines in the old version that solve a system of linear equations." % len(routines_solve_old), elapsed







