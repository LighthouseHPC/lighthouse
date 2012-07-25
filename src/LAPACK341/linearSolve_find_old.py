import os, urllib, shutil, csv
from summary import *


print "------------- Find the routines that solve a system of linear equations in the old version --------------"

old_list_url = Old_List_URL()


###------------find 'Linear Solve' and "solves the equation" routines in the old version
###------------ and write them into routines/linearSolve_old.txt
## find the routines that HAVE the keywords:
f_linearSolve_old = open('routines/linearSolve_old.txt', 'w')
routines_solve_old = []
for url in old_list_url:
    f_info = urllib.urlopen(url)
    routineName = url.split("/")[-1]
    if 'rfs.f' in routineName:
        pass
    else:
        for line in f_info:
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
print "There are %s routines in the old version that solve a system of linear equations." % len(routines_solve_old)







