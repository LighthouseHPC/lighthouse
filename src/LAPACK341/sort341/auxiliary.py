import urllib, shutil, csv
from time import time
import os
parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
os.sys.path.insert(0,parentdir) 
import summary.summary


print "----------------- Sort the auxiliary routines in v3.4.1 -----------------"


###----------- get new_list
new_list = summary.summary.New_List()



###------------ find auxiliary routines in v3.4.1 
wr_single = csv.writer(open('./routines/auxiliary_341_single.txt', 'w'), delimiter=';')
wr_double = csv.writer(open('./routines/auxiliary_341_double.txt', 'w'), delimiter=';')
wr_complex = csv.writer(open('./routines/auxiliary_341_complex.txt', 'w'), delimiter=';')
wr_complex16 = csv.writer(open('./routines/auxiliary_341_complex16.txt', 'w'), delimiter=';')
wr_aux = csv.writer(open('./routines/auxiliary_341_aux.txt', 'w'), delimiter=';')

auxiliary_single = []
auxiliary_double = []
auxiliary_complex = []
auxiliary_complex16 = []
auxiliary_aux = []
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
    if "auxiliary" in category:
        print routineName, "------->", category
        if category[0:3] == "aux":
            m += 1
            auxiliary_aux.append(routineName)
            wr_aux.writerow([m, routineName[0], routineName[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+routineName])
            #print category
        else:
            if routineName.startswith("s"):
                i += 1
                auxiliary_single.append(routineName)
                wr_single.writerow([i, routineName[0], routineName[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+routineName])
            elif routineName.startswith("d"):
                j += 1
                auxiliary_double.append(routineName)
                wr_double.writerow([j, routineName[0], routineName[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+routineName])
            elif routineName.startswith("c"):
                k += 1
                auxiliary_complex.append(routineName)
                wr_complex.writerow([k, routineName[0], routineName[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+routineName])                   
            elif routineName.startswith("z"):
                l += 1
                auxiliary_complex16.append(routineName)
                wr_complex16.writerow([l, routineName[0], routineName[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+routineName])
            else:
                print routineName, "   ", category
    else:
        pass

f.close()


print "auxiliary single: ", len(auxiliary_single)
print "auxiliary double: ", len(auxiliary_double)
print "auxiliary complex: ", len(auxiliary_complex)
print "auxiliary complex16: ", len(auxiliary_complex16)
print "auxiliary auxiliary: ", len(auxiliary_aux)
print "total time: ", time()-start
