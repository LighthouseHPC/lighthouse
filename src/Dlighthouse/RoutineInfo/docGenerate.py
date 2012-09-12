import csv, urllib
import glob
import MySQLdb
import os
dirDlighthouse = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
os.sys.path.insert(0,dirDlighthouse)


print dirDlighthouse


beginId = raw_input("Enter the start id number: ")
endId =  raw_input("Enter the end id number: ")

print "--------------------------"
print "Creating .txt files...\n"

def file_name(precision, name, i):
    return str(precision+name+"_"+i+".txt") 


### open media/docGenerate/url.csv (must be windows comma separated format)
### url.csv stores precision, routine name, and url.
reader = csv.reader(open(dirDlighthouse+"/media/docGenerate/url.csv"))



docList = []
for idn, precision, routine, url in reader:
    docList.append([precision+routine+"_"+idn+".txt"])
    
    ### make the highlight docs for the routines with idn = beginId to idn = endId
    if int(idn) in range(int(beginId), int(endId)+1):
        ### open the .html files from /media/docGenerate/Doxygen/docs/html/
        ### Note: Doxygen generated html file names have the "double underscores" problem
        if "_" in routine:
            page = open(dirDlighthouse+"/media/docGenerate/Doxygen/docs/html/"+precision+routine.replace("_", "__")+"_8f.html")
            
        else:
            page = open(dirDlighthouse+"/media/docGenerate/Doxygen/docs/html/"+precision+routine+"_8f.html")
        
        ###  save chopped info in the RoutineTxt directory.   
        copy_page= open('RoutineTxt/'+file_name(precision, routine, idn), "w")
        
        flag = 1
        while True:
            content = page.readline()
            if "Parameters" in content:
                print idn, precision+routine, "--> find match!"
                break
            
            else:
                if "Purpose:" in content:
                    flag = 0
                if "Parameters" in content:
                    flag = 1
                if not flag and not "\par Purpose:" in content:
                    content = content.replace("\n","")
                    content = content.replace("</pre> </dd></dl>","")
                    content = content.replace("   ", "")
                    copy_page.write(content)
        page.close()
        copy_page.close()
    else:
        pass


### create routine_info.csv for loading the data to table Driver_routineinfo.
f_database = csv.writer(open("routine_info.csv", "wb"))

print "\nCreating routine_info.csv for the database..."

for fileName in docList:
    f_database.writerow(fileName)
    
    
print "routine_info.csv is created!"