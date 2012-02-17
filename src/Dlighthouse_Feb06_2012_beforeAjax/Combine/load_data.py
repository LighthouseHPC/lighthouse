import csv
import MySQLdb

myDB = MySQLdb.connect("", "salin", "yell@w1234", "shark") 

cursor = myDB.cursor()


nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Combine/le_only.csv' INTO TABLE Combine_linearequation_only")




