import csv, urllib
import glob
import MySQLdb
import os
parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
os.sys.path.insert(0,parentdir)



beginId = raw_input("Enter the start id number: ")
endId =  raw_input("Enter the end id number: ")

print "--------------------------"
print "Creating .txt files...\n"

def file_name(precision, name, i):
    return str(precision+name+"_"+i+".txt") 


### open url.csv (must be windows comma separated format)
### url.csv stores precision, routine name, and url.
reader = csv.reader(open(parentdir+"/docGene/url.csv"))



docList = []
for idn, precision, routine, url in reader:
    docList.append([precision+routine+"_"+idn+".txt"])
    
    ### make the highlight docs for the routines with idn = beginId to idn = endId
    if int(idn) in range(int(beginId), int(endId)+1):
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
    else:
        pass


### create routine_info.csv for loading the data to table Driver_routineinfo.
f_database = csv.writer(open(parentdir+"/docGene/routine_info.csv", "wb"))

print "\nCreating routine_info.csv for the database..."

for fileName in docList:
    f_database.writerow(fileName)
    
    
print "routine_info.csv is created!"