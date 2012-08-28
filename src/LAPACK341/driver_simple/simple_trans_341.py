import urllib
from time import time
import os
parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
os.sys.path.insert(0,parentdir)


start = time()

print "Simple Driver routines that compute transpose:"

f_simple = open('./routines/simple_341.txt')

i = 0
for routineName in f_simple:
    url = 'http://www.netlib.org/lapack/lapack_routine/'+routineName
    f = urllib.urlopen(url)
    flag = 1
    for line in f:
        line = line[3:]
        if line.startswith("Arguments:"):
            break
        else:
            if line.startswith("\par Purpose:"):
                flag = 0
            if line.startswith("Arguments:"):
                flag = 1
            if not flag:
                index1 = line.find('A**T')
                index2 = line.find('A**H')
                if index1 > -1 or index2 > -1:
                    i += 1
                    print i,"   ", routineName
                else:
                    pass
                
f_simple.close()

elapsed = (time() - start)

print "Total time: ", elapsed