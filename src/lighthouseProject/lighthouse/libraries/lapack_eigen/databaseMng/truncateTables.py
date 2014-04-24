import MySQLdb

conn = MySQLdb.connect(host="127.0.0.1", port=3306, user="root", passwd="yellow1234", db="shark") 

cursor = conn.cursor()

cursor.execute("TRUNCATE TABLE lighthouse_lapack_eigen_standard")

cursor.execute("TRUNCATE TABLE lighthouse_lapack_eigen_generalized")

conn.commit()

cursor.close()
conn.close()

