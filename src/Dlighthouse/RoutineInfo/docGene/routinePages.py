import csv, urllib
import MySQLdb
import os
parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
os.sys.path.insert(0,parentdir)


def file_name(precision, name, i):
    return str(precision+name+"_"+i+".txt") 

### url.csv stores precision, routine name, and url.
### open url.csv (must be windows comma separated format)
reader = csv.reader(open(parentdir+"/docGene/url.csv"))

for idn, precision, routine, url in reader:
    URL = str("http://www.netlib.org/lapack/lapack_routine"+url)
    page = urllib.urlopen(URL)
    
    ###  save chopped info in the RoutineTxt directory.   
    copy_page= open(parentdir+'/docGene/RoutineTxt/'+file_name(precision, routine, idn), "w")
    
    flag = 1
    while True:
        content = page.readline()[3:]
        if "===============================================" in content:
            print idn, "--> find match!"
            break

        if "\par Further Details:" in content:
            print idn, "--> find match!"
            break
        else:
            if "\par Purpose:" in content:
                flag = 0
            if "===============================================" in content:
                flag = 1
            if not flag and not "\par Purpose:" in content:
                if "=======" in content:
                    content = ''
                if content.find("verbatim") > -1:
                    content = ''
                copy_page.write(content)
    page.close()
    copy_page.close()




