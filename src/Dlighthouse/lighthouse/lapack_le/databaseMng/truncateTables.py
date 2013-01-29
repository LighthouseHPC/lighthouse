import MySQLdb

conn = MySQLdb.connect(host="127.0.0.1", port=3306, user="root", passwd="yellow1234", db="shark") 

cursor = conn.cursor()


cursor.execute("TRUNCATE TABLE lapack_linearequation_driver")
cursor.execute("TRUNCATE TABLE lapack_linearequation_expert")
cursor.execute("TRUNCATE TABLE lapack_linearequation_simple")


cursor.execute("TRUNCATE TABLE lapack_linearequation_computational")
cursor.execute("TRUNCATE TABLE lapack_linearequation_condition_number")
cursor.execute("TRUNCATE TABLE lapack_linearequation_equilibrate")
cursor.execute("TRUNCATE TABLE lapack_linearequation_error_bound")
cursor.execute("TRUNCATE TABLE lapack_linearequation_factor")
cursor.execute("TRUNCATE TABLE lapack_linearequation_invert")      
cursor.execute("TRUNCATE TABLE lapack_linearequation_solve")


cursor.execute("TRUNCATE TABLE lapack_linearequation_only")


cursor.execute("TRUNCATE TABLE lapack_routineinfo")

conn.commit()

cursor.close()
conn.close()

