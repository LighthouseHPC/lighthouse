import MySQLdb
import os, sys, warnings

conn = MySQLdb.connect(host="127.0.0.1", port=3306, user="lighthouse", passwd="yellow1234", db="lighthousedb") 
cursor = conn.cursor()

fileDict = {
    '1': 'guided',
    '2': 'driver_standard',
    '3': 'driver_generalized',
    '4': 'computational_standard',
    '5': 'computational_generalized',
    }


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
        
      
x = raw_input('Separate with "," for dropping multiple tables. Enter \n\
              "all" for dropping all tables, or\n\
              "1" for dropping lighthouse_lapack_eigen_guided,\n\
              "2" for dropping lighthouse_lapack_eigen_driver_standard, \n\
              "3" for dropping lighthouse_lapack_eigen_driver_generalized, \n\
              "4" for dropping lighthouse_lapack_eigen_computational_standard, and\n\
              "5" for dropping lighthouse_lapack_eigen_computational_generalized. \n\
              "q" to quit the program. \n--> ')
print


x_list = x.split(',')           ##convert the input string into a list

if 'q' in x_list:
    sys.exit()
    
elif 'all' in x_list:
    for index, fileName in fileDict.items():
        table = 'lighthouse_lapack_eigen_'+fileName
        drop_table(fileName)
    
else:
    for index, fileName in fileDict.items():
        if index in x_list:
            drop_table(fileName)   


conn.commit()
cursor.close()
conn.close()

