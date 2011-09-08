#To create a csv file for loading the data to the routineinfo table.


import csv, urllib, MySQLdb, sys

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
	fileList.append([precision+routine+"_"+idn+".txt"])

x = 0
routineInfo_list = []
for item in fileList:
	routineInfo_list.append(item) 
	x += 1




writer = csv.writer(open("routine_info.csv", "wb"))

for item in routineInfo_list:
	writer.writerow(item)




