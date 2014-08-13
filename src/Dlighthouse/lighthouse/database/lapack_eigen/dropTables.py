import MySQLdb

conn = MySQLdb.connect(host="127.0.0.1", port=3306, user="lighthouse", passwd="yellow1234", db="lighthousedb") 

cursor = conn.cursor()

x = raw_input('Enter \n\
              "1" for dropping table lighthouse_lapack_eigen,\n\
              "2" for lighthouse_lapack_eigen_driver_standard,\n\
              "3" for lighthouse_lapack_eigen_driver_generalized,\n\
              "4" for lighthouse_lapack_eigen_computational_standard,\n\
              "5" for lighthouse_lapack_eigen_computational_generalized, or\n\
              "all" for dropping all lapack_eigen tables.\n --> ')


###---------------- for guided search ----------------###
if x in ["1", "all"]:
    cursor.execute("DROP TABLE IF EXISTS lighthouse_lapack_eigen")



###---------------- for advanced search ----------------###



conn.commit()
cursor.close()
conn.close()

