import csv
import MySQLdb

myDB = MySQLdb.connect(host="127.0.0.1", port=3306, user="root", passwd="yellow1234", db="shark") 

cursor = myDB.cursor()



nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Driver/le_simple.csv' INTO TABLE Driver_linearequation_simple")

nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Driver/le_expert.csv' INTO TABLE Driver_linearequation_expert")

nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Driver/le_all.csv' INTO TABLE Driver_linearequation_driver")


#nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Driver/lls.csv' INTO TABLE Driver_linearleastsquare")


#nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Driver/seig.csv' INTO TABLE Driver_symmetriceigenvalue")


#nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Driver/nonseig.csv' INTO TABLE Driver_nonsymmetriceigenvalue")


#nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Driver/eigensolver.csv' INTO TABLE Driver_eigensolver")



myDB.commit()
cursor.close()
myDB.close()

