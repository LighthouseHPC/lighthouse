import csv, urllib, MySQLdb

def file_name(precision, name, i):
    return str(precision+name+"_"+i+".txt") 


reader = csv.reader(open("url.csv"))

delete = ("SUBROUTINE", "$", "--", "..", "=======")

for idn, precision, routine, url in reader:
    URL = str("http://www.netlib.org/lapack/"+url)
    page = urllib.urlopen(URL)
    
    copy_page= open(file_name(precision, routine, idn), "w")
    
    flag = 1
    while True:
        content = page.readline()[1:]

        if "===============================================" in content:
            print idn, "--> find match!"
            break

        if "Further Details" in content:
            print idn, "--> find match!"
            break

        else:
            if "SUBROUTINE" in content:
                flag = 0
            if "Purpose" in content:
		flag = 1
	    if flag and not "Purpose" in content:
		if "=======" in content:
		    content = ''
		copy_page.write(content)
            
    page.close()
    copy_page.close()




