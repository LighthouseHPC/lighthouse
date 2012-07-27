import os, urllib, shutil, csv
from time import time
from summary import * 

print "------------- Find 'invert' routines in v3.4.1 --------------"


###----------- get new_list

new_list = New_List()
#print len(new_list)


###------------ find routines that compute the inverse of a matrix in the new version
###------------ and write them into routines/inverse_341.txt
wr = csv.writer(open('routines/inverse_341.txt', 'w'), delimiter=';')

routines_inverse = []
i=0
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
                index1 = line.find("inverse of a")
                if index1 > -1:
                    i = i+1
                    routines_inverse.append(routineName)
                    wr.writerow([i, routineName[0], routineName[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+routineName])
                else:
                    pass
    f.close()

elapsed = (time() - start)
print "There are %s routines that compute the 'inverse of a matrix' in v3.4.1." % len(routines_inverse),  elapsed


