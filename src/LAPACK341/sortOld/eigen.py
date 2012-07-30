import urllib, shutil, csv
from time import time
import os
parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
os.sys.path.insert(0,parentdir) 
import summary.summary


print "----------------- Sort the eigen routines in the old version -----------------"


###------------ find eigen routines in the old version 
wr_single = csv.writer(open('./routines/eigen_old_single.txt', 'w'), delimiter=';')
wr_double = csv.writer(open('./routines/eigen_old_double.txt', 'w'), delimiter=';')
wr_complex = csv.writer(open('./routines/eigen_old_complex.txt', 'w'), delimiter=';')
wr_complex16 = csv.writer(open('./routines/eigen_old_complex16.txt', 'w'), delimiter=';')
wr_aux = csv.writer(open('./routines/eigen_old_aux.txt', 'w'), delimiter=';')

eigen_single = []
eigen_double = []
eigen_complex = []
eigen_complex16 = []
eigen_aux = []
i=0
j=0
k=0
l=0
m=0
start = time()
f = open("./routines/routines_old_list.txt", 'r')
for line in f:
    routineName = line.split(' ')[1]
    category = line.split(' ')[2]
    #print category
    if "eigen" in category:
        print routineName, "------->", category
        if category[0:3] == "aux":
            m += 1
            eigen_aux.append(routineName)
            wr_aux.writerow([m, routineName[0], routineName[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+routineName])
            #print category
        elif category[0:4] == "real":
                i += 1
                eigen_single.append(routineName)
                wr_single.writerow([i, routineName[0], routineName[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+routineName])
        elif category[0:6] == "double":
            j += 1
            eigen_double.append(routineName)
            wr_double.writerow([j, routineName[0], routineName[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+routineName])
        elif category[0:9] == "complex16":
            l += 1
            eigen_complex16.append(routineName)
            wr_complex16.writerow([l, routineName[0], routineName[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+routineName])
        else:
            k += 1
            eigen_complex.append(routineName)
            wr_complex.writerow([k, routineName[0], routineName[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+routineName])  

f.close()


print "Eigen single: ", len(eigen_single)
print "Eigen double: ", len(eigen_double)
print "Eigen complex: ", len(eigen_complex)
print "Eigen complex16: ", len(eigen_complex16)
print "Eigen auxiliary: ", len(eigen_aux)
print "total time: ", time()-start
