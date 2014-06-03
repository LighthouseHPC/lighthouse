import MySQLdb

conn = MySQLdb.connect(host="127.0.0.1", port=3306, user="lighthouse", passwd="yellow1234", db="lighthousedb") 

cursor = conn.cursor()


cursor.execute("TRUNCATE TABLE lighthouse_lapack_le_driver")
cursor.execute("TRUNCATE TABLE lighthouse_lapack_le_expert")
cursor.execute("TRUNCATE TABLE lighthouse_lapack_le_simple")


cursor.execute("TRUNCATE TABLE lighthouse_lapack_le_computational")
cursor.execute("TRUNCATE TABLE lighthouse_lapack_le_condition_number")
cursor.execute("TRUNCATE TABLE lighthouse_lapack_le_equilibrate")
cursor.execute("TRUNCATE TABLE lighthouse_lapack_le_error_bound")
cursor.execute("TRUNCATE TABLE lighthouse_lapack_le_factor")
cursor.execute("TRUNCATE TABLE lighthouse_lapack_le_invert")      
cursor.execute("TRUNCATE TABLE lighthouse_lapack_le_solve")


cursor.execute("TRUNCATE TABLE lighthouse_lapack_le_only")

cursor.execute("TRUNCATE TABLE lighthouse_lapack_le_arg")
cursor.execute("TRUNCATE TABLE lighthouse_lapack_le_arg_c")


cursor.execute("TRUNCATE TABLE lighthouse_lapack_le_routineinfo")

conn.commit()

cursor.close()
conn.close()

