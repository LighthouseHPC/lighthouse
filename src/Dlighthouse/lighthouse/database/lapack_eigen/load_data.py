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

fileDict = {
    '1': 'guided',
    '2': 'driver_standard',
    '3': 'driver_generalized',
    '4': 'computational_standard',
    '5': 'computational_generalized',
    }


x = raw_input('Separate with ",". Enter \n\
              "1" for loading data to lighthouse_lapack_eigen_guided,\n\
              "2" to lighthouse_lapack_eigen_driver_standard, \n\
              "3" to lighthouse_lapack_eigen_driver_generalized, \n\
              "4" to lighthouse_lapack_eigen_computational_standard, \n\
              "5" to lighthouse_lapack_eigen_computational_generalized, or\n\
              "all" for loading data to all lapack_eigen tables.\n --> ')


x_list = x.split(',')           ##convert the input string into a list


for index, fileName in fileDict.items():
    if index in x_list:
        table = 'lighthouse_lapack_eigen_'+fileName
        nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '%s.csv' INTO TABLE \
                                        %s  FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'" %(fileName, table))

###---------------- for guided search ----------------###
#if x in ["1", "all"]:
#    nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'guided_eigen.csv' INTO TABLE \
#                                        lighthouse_lapack_eigen  FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'")



###---------------- for advanced search ----------------###

myDB.commit()
cursor.close()
myDB.close()

