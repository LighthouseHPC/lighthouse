import MySQLdb

myDB = MySQLdb.connect(host="127.0.0.1", port=3306, user="lighthouse", passwd="yellow1234", db="lighthousedb", local_infile = 1) 

cursor = myDB.cursor()



##### ----------------------------- IMPORTANT ---------------------------------#####
##### (1) ALL .csv files must be in the "Windows Comma Separated" format. #####
##### (2) MySQL storage engine must be set = MyISAM (old), not InnoDB (default) #####
##### -------------------------------------------------------------------------#####

nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'le_solve_all.csv' INTO \
                                     TABLE lighthouse_lapack_le_only FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'")


myDB.commit()
cursor.close()
myDB.close()

