import os, urllib, shutil, csv
from time import time
from summary import *


print "------------- Find 'invert' routines in the old version --------------"

old_list_url = Old_List_URL()


###------------find 'inverse of a matrix" routines in the old version
###------------ and write them into routines/inverse_old.txt
## find the routines that HAVE the keywords:
f_inverse_old = open('routines/inverse_old.txt', 'w')
routines_inverse_old = []
start = time()
for url in old_list_url:
    f_info = urllib.urlopen(url)
    routineName = url.split("/")[-1]
    flag = 1
    for line in f_info:
        if "================================" in line:
            break
        else:
            line = line[3:]
            if line.startswith("Purpose"):
                flag = 0
            if line.startswith("Arguments"):
                flag = 1
            if not flag:
                index1 = line.find("inverse of a")
                if index1 > -1:
                    routines_inverse_old.append(routineName)
                    f_inverse_old.write(routineName+'\n')
                else:
                    pass
    f_info.close()
    
elapsed = (time() - start)
print "There are %s routines in the old version that invert a matrix." % len(routines_inverse_old), elapsed







