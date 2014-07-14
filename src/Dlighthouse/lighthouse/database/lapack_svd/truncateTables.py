import MySQLdb

conn = MySQLdb.connect(host="127.0.0.1", port=3306, user="lighthouse", passwd="lighthousedb", db="shark") 

cursor = conn.cursor()

cursor.execute("TRUNCATE TABLE lighthouse_lapack_eigen")

conn.commit()

cursor.close()
conn.close()

