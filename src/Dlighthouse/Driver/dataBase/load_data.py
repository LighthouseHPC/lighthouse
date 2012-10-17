import MySQLdb
import os
#parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
#os.sys.path.insert(0,parentdir) 

myDB = MySQLdb.connect(host="127.0.0.1", port=3306, user="root", passwd="ecsygigh", db="shark") 

cursor = myDB.cursor()



##### ----------------------------- IMPORTANT ---------------------------------#####
##### (1) ALL .csv files must be in the "Windows Comma Separated" format. #####
##### (2) MySQL storage engine must be set = MyISAM (old), not InnoDB (default) #####
##### -------------------------------------------------------------------------#####

nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'le_simple.csv' INTO TABLE \
                                    Driver_linearequation_simple FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'")

nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'le_expert.csv' INTO TABLE \
                                    Driver_linearequation_expert FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'")

nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'le_driver_all.csv' INTO TABLE \
                                     Driver_linearequation_driver FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'")


#nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'lls.csv' INTO TABLE Driver_linearleastsquare")


#nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'seig.csv' INTO TABLE Driver_symmetriceigenvalue")


#nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'nonseig.csv' INTO TABLE Driver_nonsymmetriceigenvalue")


#nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'eigensolver.csv' INTO TABLE Driver_eigensolver")



myDB.commit()
cursor.close()
myDB.close()

