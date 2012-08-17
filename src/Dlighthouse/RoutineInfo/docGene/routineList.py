import csv, urllib, sys
import MySQLdb
import os
parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
os.sys.path.insert(0,parentdir)


#To create a csv file for loading the data to the routineinfo table.


def file_name(precision, name, i):
    return str(precision+name+"_"+i+".txt") 


reader = csv.reader(open(parentdir+"/docGene/url.csv"))

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




writer = csv.writer(open(parentdir+"/docGene/routine_info.csv", "wb"))

for item in routineInfo_list:
	writer.writerow(item)




