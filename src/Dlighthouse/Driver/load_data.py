import csv
import MySQLdb

myDB = MySQLdb.connect("", "salin", "yell@w1234", "shark") 

cursor = myDB.cursor()



#nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Driver/le_simple.csv' INTO TABLE Driver_linearequation_simple")

#nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Driver/le_expert.csv' INTO TABLE Driver_linearequation_expert")

nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Driver/le_all.csv' INTO TABLE Driver_linearequation_driver")


#nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Driver/lls.csv' INTO TABLE Driver_linearleastsquare")


#nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Driver/seig.csv' INTO TABLE Driver_symmetriceigenvalue")


#nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Driver/nonseig.csv' INTO TABLE Driver_nonsymmetriceigenvalue")


#nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Driver/eigensolver.csv' INTO TABLE Driver_eigensolver")



'''
reader = csv.reader(open("/disks/large/home/salin/Documents/Lighthouse/Dlighthouse/RoutineInfo/routine_info.csv"))

i = 1
for file in reader:
	fd = open("/disks/large/home/salin/Documents/Lighthouse/Dlighthouse/RoutineInfo/RoutineTxt/"+file[0], "r")
	myDB.query("insert into Driver_routineinfo (id, routine, info) values (\'%d\', \'%s\', \'%s\')" % (i, file[0], myDB.escape_string(fd.read())))
	i += 1

'''


