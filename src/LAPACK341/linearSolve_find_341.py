import os, urllib, shutil, csv
from compare import * 


###----------- get new_list

new_list = New_List()
#print len(new_list)


###------------ find 'Linear Solve' and "solves the equation" routines in the new version
###------------ and write them into routines/routines_341_linearSolve.txt
wr = csv.writer(open('routines/routines_341_linearSolve.txt', 'w'), delimiter=';')

routines_solve = []
i=0
for item in new_list:
    f = urllib.urlopen("http://www.netlib.org/lapack/lapack_routine/"+item)
    text = f.read()
    index1 = text.find("system of linear equations")
    if index1 > -1:
        i = i+1
        routines_solve.append(item)
        wr.writerow([i, item[0], item[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+item])
    else:
        index2 = text.find("solves the equation")
        if index2 > -1:
            i = i+1
            routines_solve.append(item)
            wr.writerow([i, item[0], item[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+item])
        else:
            index3 = text.find("system of the form")
            if index3 > -1:
                i = i+1
                routines_solve.append(item)
                wr.writerow([i, item[0], item[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+item])
            else:
                index4 = text.find("systems of equations")
                if index4 > -1:
                    i = i+1
                    routines_solve.append(item)
                    wr.writerow([i, item[0], item[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+item])
                else:
                    pass
    f.close()

print "There are %s routines that solve a system of linear equations." % len(routines_solve) 


