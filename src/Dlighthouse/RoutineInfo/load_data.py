import csv
import MySQLdb

myDB = MySQLdb.connect(host="127.0.0.1", port=3306, user="root", passwd="yellow1234", db="shark") 

cursor = myDB.cursor()


reader = csv.reader(open("/Users/salin/Documents/Lighthouse/Dlighthouse/RoutineInfo/routine_info.csv"))

i = 1
for file in reader:
	fd = open("/Users/salin/Documents/Lighthouse/Dlighthouse/RoutineInfo/RoutineTxt/"+file[0], "r")
	myDB.query("insert into Driver_routineinfo (id, routine, info) values (\'%d\', \'%s\', \'%s\')" % (i, file[0], myDB.escape_string(fd.read())))
	i += 1


myDB.commit()
cursor.close()
myDB.close()

