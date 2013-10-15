import MySQLdb

conn = MySQLdb.connect(host="127.0.0.1", port=3306, user="root", passwd="yellow1234", db="shark") 

cursor = conn.cursor()


cursor.execute("DROP TABLE IF EXISTS lighthouse_lapack_le_driver")
cursor.execute("DROP TABLE IF EXISTS lighthouse_lapack_le_expert")
cursor.execute("DROP TABLE IF EXISTS lighthouse_lapack_le_simple")

    
cursor.execute("DROP TABLE IF EXISTS lighthouse_lapack_le_computational")
cursor.execute("DROP TABLE IF EXISTS lighthouse_lapack_le_condition_number")
cursor.execute("DROP TABLE IF EXISTS lighthouse_lapack_le_equilibrate")
cursor.execute("DROP TABLE IF EXISTS lighthouse_lapack_le_error_bound")
cursor.execute("DROP TABLE IF EXISTS lighthouse_lapack_le_factor")
cursor.execute("DROP TABLE IF EXISTS lighthouse_lapack_le_invert")      
cursor.execute("DROP TABLE IF EXISTS lighthouse_lapack_le_solve")


cursor.execute("DROP TABLE IF EXISTS lighthouse_lapack_le_only")


cursor.execute("DROP TABLE IF EXISTS lighthouse_lapack_le_arg")
cursor.execute("DROP TABLE IF EXISTS lighthouse_lapack_le_arg_c")


cursor.execute("DROP TABLE IF EXISTS lighthouse_lapack_routineinfo")

conn.commit()

cursor.close()
conn.close()

