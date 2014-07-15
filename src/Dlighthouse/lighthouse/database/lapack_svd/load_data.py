import MySQLdb
import os
#parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
#os.sys.path.insert(0,parentdir) 

myDB = MySQLdb.connect(host="127.0.0.1", port=3306, user="lighthouse", passwd="yellow1234", db="lighthousedb", local_infile = 1) 

cursor = myDB.cursor()



##### ----------------------------- IMPORTANT ---------------------------------#####
##### (1) ALL .csv files must be in the "Windows Comma Separated" format. #####
##### (2) MySQL storage engine must be set = MyISAM (old), not InnoDB (default) #####
##### -------------------------------------------------------------------------#####

x = raw_input('Enter "G" for loading data to guided, "A" for loading data to advanced search table, or 'B' for both. ')



###---------------- for guided search ----------------###
if x in ["G", "B", "g", "b"]:
    nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'guided_svd.csv' INTO TABLE \
                                        lighthouse_lapack_svd  FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'")


###---------------- for advanced search ----------------###
if x in ["A", "B", "a", "b"]:
    nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'advanced_svd.csv' INTO TABLE \
                                        lighthouse_lapack_svd_advanced  FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'")



myDB.commit()
cursor.close()
myDB.close()

