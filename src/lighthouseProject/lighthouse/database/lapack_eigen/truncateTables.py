import MySQLdb
import os, sys

conn = MySQLdb.connect(host="127.0.0.1", port=3306, user="lighthouse", passwd="yellow1234", db="lighthousedb") 
cursor = conn.cursor()

fileDict = {
    '1': 'guided',
    '2': 'driver_standard',
    '3': 'driver_generalized',
    '4': 'computational_standard',
    '5': 'computational_generalized',
    }


def truncate_data(fileName):
    table = 'lighthouse_lapack_eigen_'+fileName
    try:
        cursor.execute("TRUNCATE TABLE %s"%table)
        print "Finished truncating data from %s."%table
    except Exception as e:          
        print "Error: ", e.args
        
      
x = raw_input('Separate with "," for truncating data from multiple tables. Enter \n\
              "all" for truncating data from all lapack_eigen tables, or\n\
              "1" from lighthouse_lapack_eigen_guided,\n\
              "2" from lighthouse_lapack_eigen_driver_standard, \n\
              "3" from lighthouse_lapack_eigen_driver_generalized, \n\
              "4" from lighthouse_lapack_eigen_computational_standard, and\n\
              "5" from lighthouse_lapack_eigen_computational_generalized. \n\
              "q" to quit the program. \n--> ')
print


x_list = x.split(',')           ##convert the input string into a list

if 'q' in x_list:
    sys.exit()
    
elif 'all' in x_list:
    for index, fileName in fileDict.items():
        table = 'lighthouse_lapack_eigen_'+fileName
        truncate_data(fileName)
    
else:
    for index, fileName in fileDict.items():
        if index in x_list:
            truncate_data(fileName)   


conn.commit()
cursor.close()
conn.close()

