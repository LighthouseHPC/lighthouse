import urllib, shutil, csv
from time import time
import os
parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
os.sys.path.insert(0,parentdir) 
import summary.summary


print "----------------- Sort the computational routines in v3.4.1 -----------------"


###----------- get new_list
new_list = summary.summary.New_List()



###------------ find computational routines in v3.4.1 
wr_single = csv.writer(open('./routines/computational_341_single.txt', 'w'), delimiter=';')
wr_double = csv.writer(open('./routines/computational_341_double.txt', 'w'), delimiter=';')
wr_complex = csv.writer(open('./routines/computational_341_complex.txt', 'w'), delimiter=';')
wr_complex16 = csv.writer(open('./routines/computational_341_complex16.txt', 'w'), delimiter=';')
wr_aux = csv.writer(open('./routines/computational_341_aux.txt', 'w'), delimiter=';')

computational_single = []
computational_double = []
computational_complex = []
computational_complex16 = []
computational_aux = []
i=0
j=0
k=0
l=0
m=0
start = time()
for routineName in new_list:
    f = urllib.urlopen("http://www.netlib.org/lapack/lapack_routine/"+routineName)
    text = f.read()
    text = text.split("\n")
    for line in text:
        line = line[3:]
        if line.startswith("\ingroup"):
            category = line.split(" ")[1]
            if "computational" in category:
                print routineName, "------->", category
                if "real" in category:
                    i += 1
                    computational_single.append(routineName)
                    wr_single.writerow([i, routineName[0], routineName[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+routineName])
                elif "double" in category:
                    j += 1
                    computational_double.append(routineName)
                    wr_double.writerow([j, routineName[0], routineName[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+routineName])
                elif "complex" in category:
                    k += 1
                    computational_complex.append(routineName)
                    wr_complex.writerow([k, routineName[0], routineName[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+routineName])                   
                elif "complex16" in category:
                    l += 1
                    computational_complex16.append(routineName)
                    wr_complex16.writerow([l, routineName[0], routineName[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+routineName])
                elif "aux" in category:
                    m += 1
                    computational_complex16.append(routineName)
                    wr_aux.writerow([m, routineName[0], routineName[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+routineName])
                else:
                    print routineName, "   ", category
                    
            else:
                pass
        else:
            pass

    f.close()


print "Computational single: ", len(computational_single)
print "Computational double: ", len(computational_double)
print "Computational complex: ", len(computational_complex)
print "Computational complex16: ", len(computational_complex16)
print "Computational auxiliary: ", len(computational_aux)
print "total time: ", time()-start
