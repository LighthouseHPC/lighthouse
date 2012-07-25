import os, urllib, shutil, csv
from compare_linearSolve import * 


###----------- get new_list

new_list = New_List()
#print len(new_list)


###------------ find 'Linear Solve' and "solves the equation" routines in the new version
###------------ and write them into routines/routines_341_linearSolve.txt
wr = csv.writer(open('routines/linearSolve_341.txt', 'w'), delimiter=';')

routines_solve = []
i=0
for routineName in new_list:
    if 'rfs.f' in routineName:
        pass
    else:
        f = urllib.urlopen("http://www.netlib.org/lapack/lapack_routine/"+routineName)
        text = f.read()
        index1 = text.find("system of linear")
        if index1 > -1:
            i = i+1
            routines_solve.append(routineName)
            wr.writerow([i, routineName[0], routineName[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+routineName])
        else:
            index2 = text.find("solves the equation")
            if index2 > -1:
                i = i+1
                routines_solve.append(routineName)
                wr.writerow([i, routineName[0], routineName[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+routineName])
            else:
                index3 = text.find("system of the form")
                if index3 > -1:
                    i = i+1
                    routines_solve.append(routineName)
                    wr.writerow([i, routineName[0], routineName[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+routineName])
                else:
                    index4 = text.find("systems of equations")
                    if index4 > -1:
                        i = i+1
                        routines_solve.append(routineName)
                        wr.writerow([i, routineName[0], routineName[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+routineName])
                    else:
                        pass
    f.close()

print "There are %s routines in v3.4.1 that solve a system of linear equations." % len(routines_solve) 


