import csv
import MySQLdb

myDB = MySQLdb.connect(host="127.0.0.1", port=3306, user="root", passwd="yellow1234", db="shark") 

cursor = myDB.cursor()


nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Combine/le_only.csv' INTO TABLE Combine_linearequation_only")


myDB.commit()
cursor.close()
myDB.close()

