import MySQLdb
import os, sys, warnings

myDB = MySQLdb.connect(host="127.0.0.1", port=3306, user="lighthouse", passwd="yellow1234", db="lighthousedb", local_infile = 1) 
cursor = myDB.cursor()

##### ----------------------------- IMPORTANT ---------------------------------#####
##### (1) ALL .csv files must be in the "Windows Comma Separated" format. #####
##### (2) MySQL storage engine must be set = MyISAM (old), not InnoDB (default) #####
##### -------------------------------------------------------------------------#####
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

def truncate_data(fileName):
    table = 'lighthouse_lapack_eigen_'+fileName
    try:
        cursor.execute("TRUNCATE TABLE %s"%table)
        print "Finished truncating data from %s."%table
    except Exception as e:          
        print "Error: ", e.args
        
        
def drop_table(fileName):
    table = 'lighthouse_lapack_eigen_'+fileName
    try:
        with warnings.catch_warnings(record=True) as w:
            cursor.execute("DROP TABLE IF EXISTS %s"%table)
            if len(w):
                print "Warning:", w[0].message 
            else:
                print "Finished dropping table %s."%table

    except Exception as e:          
        print "Error: ", e.args
        


dispatcher={
    'l': load_data,
    't': truncate_data,
    'd': drop_table
    }
  
fileDict = {
    '1': 'guided',
    '2': 'driver_standard_sh',
    '3': 'driver_standard_g',
    '4': 'driver_generalized_sh',
    '5': 'driver_generalized_g',
    '6': 'computational_standard_sh',
    '7': 'computational_standard_g',    
    '8': 'computational_generalized_sh',
    '9': 'computational_generalized_g',
    }


job = raw_input('What would you like to do? Enter \n\
                "l" to load data, \n\
                "t" to truncate data,\n\
                "d" to drop table(s), or \n\
                "q" to quit the program. \n--> ')
print


if job == 'l':
    x = raw_input('Separate with "," if loading data to multiple tables. Enter \n\
                  "all" for loading data to all lapack_eigen tables, or\n\
                  "1" to lighthouse_lapack_eigen_guided,\n\
                  "2" to lighthouse_lapack_eigen_driver_standard_sh, \n\
                  "3" to lighthouse_lapack_eigen_driver_standard_g, \n\
                  "4" to lighthouse_lapack_eigen_driver_generalized_sh, \n\
                  "5" to lighthouse_lapack_eigen_driver_generalized_g, \n\
                  "6" to lighthouse_lapack_eigen_computational_standard_sh, \n\
                  "7" to lighthouse_lapack_eigen_computational_standard_g, \n\
                  "8" to lighthouse_lapack_eigen_computational_generalized_sh, and \n\
                  "9" to lighthouse_lapack_eigen_computational_generalized_g. \n\
                  "q" to quit the program. \n--> ')
    jobfunction=dispatcher['l']

elif job == 't':
    x = raw_input('Separate with "," if truncating data from multiple tables. Enter \n\
                  "all" for truncating data from all lapack_eigen tables, or\n\
                  "1" from lighthouse_lapack_eigen_guided,\n\
                  "2" from lighthouse_lapack_eigen_driver_standard_sh, \n\
                  "3" from lighthouse_lapack_eigen_driver_standard_g, \n\
                  "4" from lighthouse_lapack_eigen_driver_generalized_sh, \n\
                  "5" from lighthouse_lapack_eigen_driver_generalized_g, \n\
                  "6" from lighthouse_lapack_eigen_computational_standard_sh, \n\
                  "7" from lighthouse_lapack_eigen_computational_standard_g, \n\
                  "8" from lighthouse_lapack_eigen_computational_generalized_sh, and \n\
                  "9" from lighthouse_lapack_eigen_computational_generalized_g. \n\
                  "q" to quit the program. \n--> ')
    jobfunction=dispatcher['t']
    
elif job == 'd':
    x = raw_input('Separate with "," if dropping multiple tables. Enter \n\
                  "all" for dropping all tables, or\n\
                  "1" for dropping lighthouse_lapack_eigen_guided,\n\
                  "2" for dropping lighthouse_lapack_eigen_driver_standard_sh, \n\
                  "3" for dropping lighthouse_lapack_eigen_driver_standard_g, \n\
                  "4" for dropping lighthouse_lapack_eigen_driver_generalized_sh, \n\
                  "5" for dropping lighthouse_lapack_eigen_driver_generalized_g, \n\
                  "6" for dropping lighthouse_lapack_eigen_computational_standard_sh, \n\
                  "7" for dropping lighthouse_lapack_eigen_computational_standard_g, \n\
                  "8" for dropping lighthouse_lapack_eigen_computational_generalized_sh, and \n\
                  "9" for dropping lighthouse_lapack_eigen_computational_generalized_g. \n\
                  "q" to quit the program. \n--> ')
    jobfunction=dispatcher['d']

elif job == 'q':
    sys.exit()
    
else:
    print "Invalid input!"
    sys.exit()

    
print


x_list = x.split(',')           ##convert the input string into a list

if 'q' in x_list:
    sys.exit()
    
elif 'all' in x_list:
    for index, fileName in fileDict.items():
        table = 'lighthouse_lapack_eigen_'+fileName
        jobfunction(fileName)
    
else:
    for index, fileName in fileDict.items():
        if index in x_list:
            jobfunction(fileName)           


myDB.commit()
cursor.close()
myDB.close()

