import MySQLdb

conn = MySQLdb.connect(host="127.0.0.1", port=3306, user="lighthouse", passwd="yellow1234", db="lighthousedb") 

cursor = conn.cursor()


x = raw_input('Enter \n\
              "1" for truncating data from lighthouse_lapack_eigen,\n\
              "2" to lighthouse_lapack_eigen_driver_standard,\n\
              "3" to lighthouse_lapack_eigen_driver_generalized,\n\
              "4" to lighthouse_lapack_eigen_computational_standard,\n\
              "5" to lighthouse_lapack_eigen_computational_generalized, or\n\
              "all" for truncating data from all lapack_eigen tables.\n --> ')


###---------------- for guided search ----------------###
if x in ["1", "all"]:
    cursor.execute("TRUNCATE TABLE lighthouse_lapack_eigen")



###---------------- for advanced search ----------------###



conn.commit()
cursor.close()
conn.close()

