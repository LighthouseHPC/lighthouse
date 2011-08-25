import csv
import MySQLdb

myDB = MySQLdb.connect("", "salin", "yell@w1234", "shark") 

cursor = myDB.cursor()


nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Computational/problem.csv' INTO TABLE Computational_problem")


nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Computational/eigensolver_comp.csv' INTO TABLE Computational_eigensolver_comput")




reader = csv.reader(open("/disks/large/home/salin/Documents/Lighthouse/Dlighthouse/Computational/routine_info.csv"))

i = 1
for file in reader:
	fd = open("/disks/large/home/salin/Documents/Lighthouse/Dlighthouse/Computational/Routines/routineInfo/"+file[0], "r")
	myDB.query("insert into Computational_routineinfo_comput (id, routine, info) values (\'%d\', \'%s\', \'%s\')" % (i, file[0], myDB.escape_string(fd.read())))
	i += 1




