import urllib, shutil, csv
from time import time
import os
parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
os.sys.path.insert(0,parentdir) 
import summary.summary


print "----------------- Fild all the routines in the old version -----------------"


###----------- get new_list
old_list = summary.summary.Old_List()



###------------ find all routines the old version 
wr_all = csv.writer(open('./routines/routines_old_all.txt', 'w'), delimiter=';')
wr_list = csv.writer(open('./routines/routines_old_list.txt', 'w'), delimiter=' ')
routines_all = []

i=0
start = time()
for routineName in old_list:
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

print "Number of routine in the old version : ", len(routines_all)

print "total time: ", time()-start
