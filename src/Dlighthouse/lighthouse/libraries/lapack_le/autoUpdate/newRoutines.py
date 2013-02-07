import os, urllib, shutil

os.system("wget -O routines.html http://www.netlib.org/lapack/lapack_routine/")
f = open("routines.html")

for line in f:
    fileName = line.split("\"")[1]
    print fileName[:-3] 

f.close()



