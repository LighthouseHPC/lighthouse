import csv
import MySQLdb

myDB = MySQLdb.connect(host="127.0.0.1", port=3306, user="root", passwd="yellow1234", db="shark") 

cursor = myDB.cursor()



nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Computational/le_computational_all.csv' INTO TABLE Computational_linearequation_computational")


nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Computational/le_factor.csv' INTO TABLE Computational_linearequation_factor")


nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Computational/le_solve.csv' INTO TABLE Computational_linearequation_solve")


nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Computational/le_condition_number.csv' INTO TABLE Computational_linearequation_condition_number")


nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Computational/le_error_bounds.csv' INTO TABLE Computational_linearequation_error_bound")


nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Computational/le_invert.csv' INTO TABLE Computational_linearequation_invert")


nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Computational/le_equilibrate.csv' INTO TABLE Computational_linearequation_equilibrate")


#nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Computational/eigensolver_comp.csv' INTO TABLE Computational_eigensolver_comput")


myDB.commit()
cursor.close()
myDB.close()


