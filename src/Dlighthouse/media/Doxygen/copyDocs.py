import csv, urllib

beginId = raw_input("Enter the start id number: ")
endId =  raw_input("Enter the end id number: ")

print "--------------------------"
print "Copying .f files from http://www.netlib.org/lapack/lapack_routine/...\n"



### make file names
def file_name(precision, name, i):
    return str(precision+name+".f") 



### open url.csv (must be "windows comma separated" format)
### url.csv stores precision, routine name, and url.
reader = csv.reader(open("url.csv"))



###  save documentation (fortran) files in the 'fortran' directory.  
for idn, precision, routine, url in reader:    
    if int(idn) in range(int(beginId), int(endId)+1):
        URL = str("http://www.netlib.org/lapack/lapack_routine"+url)
        page = urllib.urlopen(URL)
        copy_page= open('lapack/'+file_name(precision, routine, idn), "w")
        
        print idn, "  ", precision, routine      
        content = page.read()
        copy_page.write(content)
        
        page.close()
        copy_page.close()
    else:
        pass

print "--------------------------"
print "Completed copying documentation files to Doxygen!"


