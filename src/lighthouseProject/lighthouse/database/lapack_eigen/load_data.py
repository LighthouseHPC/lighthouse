import MySQLdb
import os, sys, warnings

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


def load_data(fileName):
    table = 'lighthouse_lapack_eigen_'+fileName
    try:
        with warnings.catch_warnings(record=True) as w:
            nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE '%s.csv' INTO TABLE \
                                            %s  FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'" %(fileName, table))
            if len(w):
                print "Warning:", w[0].message 
            else:
                print "Finished loading data to %s."%table
    except Exception as e:          
        print "Error: ", e.args
        
      
x = raw_input('Separate with "," for loading data to multiple tables. Enter \n\
              "all" for loading data to all lapack_eigen tables, or\n\
              "1" to lighthouse_lapack_eigen_guided,\n\
              "2" to lighthouse_lapack_eigen_driver_standard, \n\
              "3" to lighthouse_lapack_eigen_driver_generalized, \n\
              "4" to lighthouse_lapack_eigen_computational_standard, and\n\
              "5" to lighthouse_lapack_eigen_computational_generalized. \n\
              "q" to quit the program. \n--> ')
print


x_list = x.split(',')           ##convert the input string into a list

if 'q' in x_list:
    sys.exit()
    
elif 'all' in x_list:
    for index, fileName in fileDict.items():
        table = 'lighthouse_lapack_eigen_'+fileName
        load_data(fileName)
    
else:
    for index, fileName in fileDict.items():
        if index in x_list:
            load_data(fileName)           


myDB.commit()
cursor.close()
myDB.close()

