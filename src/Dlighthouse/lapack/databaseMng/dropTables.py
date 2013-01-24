import MySQLdb

conn = MySQLdb.connect(host="127.0.0.1", port=3306, user="root", passwd="yellow1234", db="shark") 

cursor = conn.cursor()


cursor.execute("DROP TABLE IF EXISTS Driver_linearequation_driver")
cursor.execute("DROP TABLE IF EXISTS Driver_linearequation_expert")
cursor.execute("DROP TABLE IF EXISTS Driver_linearequation_simple")

    
cursor.execute("DROP TABLE IF EXISTS Computational_linearequation_computational")
cursor.execute("DROP TABLE IF EXISTS Computational_linearequation_condition_number")
cursor.execute("DROP TABLE IF EXISTS Computational_linearequation_equilibrate")
cursor.execute("DROP TABLE IF EXISTS Computational_linearequation_error_bound")
cursor.execute("DROP TABLE IF EXISTS Computational_linearequation_factor")
cursor.execute("DROP TABLE IF EXISTS Computational_linearequation_invert")      
cursor.execute("DROP TABLE IF EXISTS Computational_linearequation_solve")


cursor.execute("DROP TABLE IF EXISTS Combine_linearequation_only")


cursor.execute("DROP TABLE IF EXISTS Driver_routineinfo")

conn.commit()

cursor.close()
conn.close()

