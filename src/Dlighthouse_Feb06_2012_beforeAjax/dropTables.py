import MySQLdb

conn = MySQLdb.connect("", "salin", "yell@w1234", "shark") 

cursor = conn.cursor()

#cursor.execute("DROP TABLE IF EXISTS Driver_eigensolver")
#cursor.execute("DROP TABLE IF EXISTS Driver_linearequation_driver")
#cursor.execute("DROP TABLE IF EXISTS Driver_linearequation_expert")
#cursor.execute("DROP TABLE IF EXISTS Driver_linearequation_simple")
#cursor.execute("DROP TABLE IF EXISTS Driver_linearleastsquare")
#cursor.execute("DROP TABLE IF EXISTS Driver_routineinfo")                  

    
#cursor.execute("DROP TABLE IF EXISTS Computational_linearequation_computational")
#cursor.execute("DROP TABLE IF EXISTS Computational_linearequation_condition_number")
#cursor.execute("DROP TABLE IF EXISTS Computational_linearequation_equilibrate")
#cursor.execute("DROP TABLE IF EXISTS Computational_linearequation_error_bound")
#cursor.execute("DROP TABLE IF EXISTS Computational_linearequation_factor")
#cursor.execute("DROP TABLE IF EXISTS Computational_linearequation_invert")      
#cursor.execute("DROP TABLE IF EXISTS Computational_linearequation_solve")


cursor.execute("DROP TABLE IF EXISTS Combine_linearequation_only")


conn.commit()

cursor.close()
