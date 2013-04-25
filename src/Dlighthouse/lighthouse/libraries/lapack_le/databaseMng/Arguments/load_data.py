import MySQLdb
import os
#parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
#os.sys.path.insert(0,parentdir) 

myDB = MySQLdb.connect(host="127.0.0.1", port=3306, user="root", passwd="yellow1234", db="shark") 

cursor = myDB.cursor()



##### ----------------------------- IMPORTANT ---------------------------------#####
##### MySQL storage engine must be set = MyISAM (old), not InnoDB (default) #####
##### -------------------------------------------------------------------------#####

nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'le_arguments.txt' INTO TABLE \
                                    lighthouse_lapack_le_arg FIELDS TERMINATED BY '\t' LINES TERMINATED BY '\r'")



myDB.commit()
cursor.close()
myDB.close()

