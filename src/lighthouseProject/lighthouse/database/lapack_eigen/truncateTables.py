import MySQLdb

conn = MySQLdb.connect(host="127.0.0.1", port=3306, user="lighthouse", passwd="yellow1234", db="lighthousedb") 

cursor = conn.cursor()

cursor.execute("TRUNCATE TABLE lighthouse_lapack_eigen")

cursor.execute("TRUNCATE TABLE lighthouse_lapack_sylvester")

conn.commit()

cursor.close()
conn.close()

