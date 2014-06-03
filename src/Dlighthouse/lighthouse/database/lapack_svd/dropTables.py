import MySQLdb

conn = MySQLdb.connect(host="127.0.0.1", port=3306, user="root", passwd="yellow1234", db="shark") 

cursor = conn.cursor()

cursor.execute("DROP TABLE IF EXISTS lighthouse_lapack_eigen")

cursor.execute("DROP TABLE IF EXISTS lighthouse_lapack_sylvester")

cursor.execute("DROP TABLE IF EXISTS lighthouse_lapack_routineinfo")

conn.commit()

cursor.close()
conn.close()

