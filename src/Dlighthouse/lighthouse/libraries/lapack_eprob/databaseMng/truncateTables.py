import MySQLdb

conn = MySQLdb.connect(host="127.0.0.1", port=3306, user="root", passwd="yellow1234", db="shark") 

cursor = conn.cursor()

cursor.execute("TRUNCATE TABLE lighthouse_lapack_eprob_simple")

cursor.execute("TRUNCATE TABLE lapack_eprob_RoutineInfo")

conn.commit()

cursor.close()
conn.close()

