import sqlite

conn = lite.connect(host="127.0.0.1", port=8000, db="lighthouse_orthg") 

cursor = conn.cursor()

cursor.execute("DROP TABLE IF EXISTS lighthouse_orthg")

#cursor.execute("DROP TABLE IF EXISTS lighthouse_lapack_sylvester")

#cursor.execute("DROP TABLE IF EXISTS lighthouse_lapack_routineinfo")

conn.commit()

cursor.close()
conn.close()

