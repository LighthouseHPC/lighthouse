import csv
import sqlite3 as lite

myDB = lite.connect('orthg.db') 

cursor = myDB.cursor()


reader = csv.reader(open("routine_info.csv"))

ID = raw_input('To load all data, enter "all"; otherwise, enter the ID number you wish to start loading:	')

print 'Your input is ' + ID


if ID == 'all':
	i = 1
else:
	i = int(ID)
	
for file in reader:
	fd = open("RoutineTxt/"+file[0], "r")
	myDB.query("insert into lighthouse_lapack_routineinfo (id, routine, info) values (\'%d\', \'%s\', \'%s\')" % (i, file[0], myDB.escape_string(fd.read())))
	i += 1


myDB.commit()
cursor.close()
myDB.close()

