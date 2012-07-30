import urllib, shutil, csv
from time import time
import os
parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
os.sys.path.insert(0,parentdir) 
import summary.summary


print "----------------- Sort the solve routines in v3.4.1 -----------------"


###----------- get new_list
new_list = summary.summary.New_List()



###------------ find solve routines in v3.4.1 
wr_single = csv.writer(open('./routines/solve_341_single.txt', 'w'), delimiter=';')
wr_double = csv.writer(open('./routines/solve_341_double.txt', 'w'), delimiter=';')
wr_complex = csv.writer(open('./routines/solve_341_complex.txt', 'w'), delimiter=';')
wr_complex16 = csv.writer(open('./routines/solve_341_complex16.txt', 'w'), delimiter=';')
wr_aux = csv.writer(open('./routines/solve_341_aux.txt', 'w'), delimiter=';')

solve_single = []
solve_double = []
solve_complex = []
solve_complex16 = []
solve_aux = []
i=0
j=0
k=0
l=0
m=0
start = time()
f = open("./routines/routines_341_list.txt", 'r')
for line in f:
    routineName = line.split(' ')[1]
    category = line.split(' ')[2]
    #print category
    if "solve" in category:
        print routineName, "------->", category
        if category[0:3] == "aux":
            m += 1
            solve_aux.append(routineName)
            wr_aux.writerow([m, routineName[0], routineName[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+routineName])
            #print category
        elif category[0:4] == "real":
                i += 1
                solve_single.append(routineName)
                wr_single.writerow([i, routineName[0], routineName[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+routineName])
        elif category[0:6] == "double":
            j += 1
            solve_double.append(routineName)
            wr_double.writerow([j, routineName[0], routineName[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+routineName])
        elif category[0:9] == "complex16":
            l += 1
            solve_complex16.append(routineName)
            wr_complex16.writerow([l, routineName[0], routineName[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+routineName])
        else:
            k += 1
            solve_complex.append(routineName)
            wr_complex.writerow([k, routineName[0], routineName[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+routineName])  

f.close()


print "Solve single: ", len(solve_single)
print "Solve double: ", len(solve_double)
print "Solve complex: ", len(solve_complex)
print "Solve complex16: ", len(solve_complex16)
print "Solve auxiliary: ", len(solve_aux)
print "total time: ", time()-start
