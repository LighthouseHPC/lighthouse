import urllib, shutil, csv
from time import time
import os
parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
os.sys.path.insert(0,parentdir) 
import summary.summary


print "----------------- Fild all the routines in v3.4.1 -----------------"


###----------- get new_list
new_list = summary.summary.New_List()



###------------ find sing routines in v3.4.1 
wr_all = csv.writer(open('./routines/routines_341_all.txt', 'w'), delimiter=';')
wr_list = csv.writer(open('./routines/routines_341_list.txt', 'w'), delimiter=' ')
routines_all = []

i=0
start = time()
for routineName in new_list:
    f = urllib.urlopen("http://www.netlib.org/lapack/lapack_routine/"+routineName)
    text = f.read()
    text = text.split("\n")
    for line in text:
        line = line[3:]
        if "=============================================================" in line:
            break
        else:
            if line.startswith("\ingroup"):
                i += 1
                category = line.split(" ")[1]
                routines_all.append(routineName)
                print i, " ", routineName, " ", category
                wr_all.writerow([i, routineName[0], routineName[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+routineName])
                wr_list.writerow([i, routineName, category])
            else:
                pass

    f.close()
    
print "================================================"  

dups = [x for x in routines_all if routines_all.count(x) > 1]
print "Duplicate elements:"
for item in dups:
    print item
print "================================================"


routines_all = [x for x in routines_all if not routines_all.count(x) > 1]

print "Number of routine in v3.4.1 : ", len(routines_all)

print "total time: ", time()-start
