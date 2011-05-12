import csv
import MySQLdb

myDB = MySQLdb.connect("", "salin", "fr33lh", "shark") 

cursor = myDB.cursor()


nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Drivers/problem.csv' INTO TABLE Drivers_problem")


nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Drivers/le.csv' INTO TABLE Drivers_linearequation")

nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Drivers/lls.csv' INTO TABLE Drivers_linearleastsquare")


nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Drivers/seig.csv' INTO TABLE Drivers_symmetriceigenvalue")


nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Drivers/nonseig.csv' INTO TABLE Drivers_nonsymmetriceigenvalue")





reader = csv.reader(open("/disks/large/home/salin/Documents/Lighthouse/Dlighthouse/Drivers/routine_info.csv"))

i = 1
for file in reader:
	fd = open("/disks/large/home/salin/Documents/Lighthouse/Dlighthouse/Drivers/Routines/routineInfo/"+file[0], "r")
	myDB.query("insert into Drivers_routineinfo (id, routine, info) values (\'%d\', \'%s\', \'%s\')" % (i, file[0], myDB.escape_string(fd.read())))
	i += 1




