import MySQLdb
import os

myDB = MySQLdb.connect(host="127.0.0.1", port=3306, user="lighthouse", passwd="yellow1234", db="lighthousedb", local_infile = 1)
cursor = myDB.cursor()

##### ----------------------------- IMPORTANT ---------------------------------#####
##### (1) ALL .csv files must be in the "Windows Comma Separated" format. #####
##### (2) MySQL storage engine must be set = MyISAM (old), not InnoDB (default) #####
##### -------------------------------------------------------------------------#####

fr = open('slepc_treeright.sql','r')
fl = open('slepc_treeleft.sql','r')
for liner in fr:
	cursor.execute(liner)
for linel in fl:
	cursor.execute(linel)

myDB.commit()
cursor.close()
myDB.close()
