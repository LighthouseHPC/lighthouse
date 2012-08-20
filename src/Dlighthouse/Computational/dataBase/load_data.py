import MySQLdb

myDB = MySQLdb.connect(host="127.0.0.1", port=3306, user="root", passwd="yellow1234", db="shark") 

cursor = myDB.cursor()



##### ----------------------------- IMPORTANT ---------------------------------#####
##### (1) ALL .csv files must be in the "Windows Comma Separated" format. #####
##### (2) MySQL storage engine must be set = MyISAM (old), not InnoDB (default) #####
##### -------------------------------------------------------------------------#####

nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'le_computational_all.csv' INTO \
                                     TABLE Computational_linearequation_computational FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'")


#nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'le_factor.csv' INTO \
#                                     TABLE Computational_linearequation_factor FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'")
#
#
#nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'le_solve.csv' INTO \
#                                     TABLE Computational_linearequation_solve FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'")
#
#
#nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'le_condition_number.csv' INTO \
#                                     TABLE Computational_linearequation_condition_number FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'")
#
#
#nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'le_error_bounds.csv' INTO \
#                                     TABLE Computational_linearequation_error_bound FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'")
#
#
#nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'le_invert.csv' INTO \
#                                     TABLE Computational_linearequation_invert FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'")
#
#
#nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'le_equilibrate.csv' INTO \
#                                     TABLE Computational_linearequation_equilibrate FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'")


#nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '~/Documents/Lighthouse/Dlighthouse/Computational/eigensolver_comp.csv' INTO TABLE Computational_eigensolver_comput")


myDB.commit()
cursor.close()
myDB.close()


