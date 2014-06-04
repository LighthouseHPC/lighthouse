import MySQLdb

myDB = MySQLdb.connect(host="127.0.0.1", port=3306, user="lighthouse", passwd="yellow1234", db="lighthousedb", local_infile = 1) 

cursor = myDB.cursor()



##### ----------------------------- IMPORTANT ---------------------------------#####
##### (1) ALL .csv files must be in the "Windows Comma Separated" format. #####
##### (2) MySQL storage engine must be set = MyISAM (old), not InnoDB (default) #####
##### -------------------------------------------------------------------------#####

nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'le_computational_all.csv' INTO \
                                     TABLE lighthouse_lapack_le_computational FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'")


#nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'le_factor.csv' INTO \
#                                     TABLE lighthouse_lapack_le_factor FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'")


nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'le_solve.csv' INTO \
                                     TABLE lighthouse_lapack_le_solve FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'")


#nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'le_condition_number.csv' INTO \
#                                     TABLE lighthouse_lapack_le_condition_number FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'")


#nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'le_error_bounds.csv' INTO \
#                                     TABLE lighthouse_lapack_le_error_bound FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'")


#nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'le_invert.csv' INTO \
#                                     TABLE lighthouse_lapack_le_inverse FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'")


#nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'le_equilibrate.csv' INTO \
#                                     TABLE lighthouse_lapack_le_equilibrate FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'")



myDB.commit()
cursor.close()
myDB.close()


