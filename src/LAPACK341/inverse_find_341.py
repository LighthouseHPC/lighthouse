import os, urllib, shutil, csv
from summary import * 

print "------------- Find the routines that solve a system of linear equations in v3.4.1 --------------"


###----------- get new_list

new_list = New_List()
#print len(new_list)


###------------ find routines that compute the inverse of a matrix in the new version
###------------ and write them into routines/routines_341_linearSolve.txt
wr = csv.writer(open('routines/inverse_341.txt', 'w'), delimiter=';')

routines_inverse = []
i=0
for routineName in new_list:
    if 'rfs.f' in routineName:
        pass
    else:
        f = urllib.urlopen("http://www.netlib.org/lapack/lapack_routine/"+routineName)
        text = f.read()
        index1 = text.find("inverse of a")
        if index1 > -1:
            i = i+1
            routines_inverse.append(routineName)
            wr.writerow([i, routineName[0], routineName[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+routineName])
        else:
            pass
    f.close()

print "There are %s routines that compute the inverse of a matrix in v3.4.1." % len(routines_inverse) 


