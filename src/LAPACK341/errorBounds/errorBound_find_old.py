import urllib, shutil, csv
from time import time
import os
parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
os.sys.path.insert(0,parentdir) 
import summary


print "------------- Find 'error bounds' routines in the old version --------------"

old_list_url = summary.Old_List_URL()


###------------find 'error bounds" routines in the old version
###------------ and write them into routines/errorBound_old.txt
## find the routines that HAVE the keywords:
f_errorBound_old = open('routines/errorBound_old.txt', 'w')
routines_errorBound_old = []
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
                index1 = line.find("error bounds")
                if index1 > -1:
                    routines_errorBound_old.append(routineName)
                    f_errorBound_old.write(routineName+'\n')
                else:
                    pass
    f_info.close()
    
elapsed = (time() - start)
print "There are %s routines in the old version that provides error bounds." % len(routines_errorBound_old), elapsed







