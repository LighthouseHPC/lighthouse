import MySQLdb
import os
#parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
#os.sys.path.insert(0,parentdir) 

myDB = MySQLdb.connect(host="127.0.0.1", port=3306, user="root", passwd="yellow1234", db="shark", local_infile = 1) 

cursor = myDB.cursor()



##### ----------------------------- IMPORTANT ---------------------------------#####
##### (1) ALL .csv files must be in the "Windows Comma Separated" format. #####
##### (2) MySQL storage engine must be set = MyISAM (old), not InnoDB (default) #####
##### -------------------------------------------------------------------------#####

nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'guided_eigen.csv' INTO TABLE \
                                    lighthouse_lapack_eigen  FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'")


#nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'guided_generalized.csv' INTO TABLE \
#                                    lighthouse_lapack_eigen_generalized  FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'")




myDB.commit()
cursor.close()
myDB.close()

