import csv, urllib, MySQLdb

def file_name(precision, name, i):
    return str(precision+name+"_"+i+".txt") 


reader = csv.reader(open("url.csv"))

idList = []
precisionList = []
routineList = []
urlList = []
fileList = []

for idn, precision, routine, url in reader:
	idList.append(idn)
	precisionList.append(precision)
	routineList.append(routine)
	urlList.append(url)
	fileList.append(precision+routine+"_"+idn+".txt")

x = 0
for item in idList:
	print item, precisionList[x], routineList[x], urlList[x], fileList[x] 
	x += 1



