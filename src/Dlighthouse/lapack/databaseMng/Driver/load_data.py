import MySQLdb
import os
#parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
#os.sys.path.insert(0,parentdir) 

myDB = MySQLdb.connect(host="127.0.0.1", port=3306, user="root", passwd="yellow1234", db="shark") 

cursor = myDB.cursor()



##### ----------------------------- IMPORTANT ---------------------------------#####
##### (1) ALL .csv files must be in the "Windows Comma Separated" format. #####
##### (2) MySQL storage engine must be set = MyISAM (old), not InnoDB (default) #####
##### -------------------------------------------------------------------------#####

nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'le_simple.csv' INTO TABLE \
                                    lapack_linearequation_simple FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'")

nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'le_expert.csv' INTO TABLE \
                                    lapack_linearequation_expert FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'")

nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'le_driver_all.csv' INTO TABLE \
                                     lapack_linearequation_driver FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'")



myDB.commit()
cursor.close()
myDB.close()

