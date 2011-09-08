import csv
import MySQLdb

myDB = MySQLdb.connect("", "salin", "yell@w1234", "shark") 

cursor = myDB.cursor()



nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Combine/le_comb.csv' INTO TABLE Combine_linearequation_comb")


nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Combine/le_transpose.csv' INTO TABLE Combine_linearequation_trans")



#nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Computational/eigensolver_comp.csv' INTO TABLE Computational_eigensolver_comput")





