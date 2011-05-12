# Make one table for one .txt file --> 61 rows --> bad!!

import MySQLdb

conn = MySQLdb.connect("hooray", "salin", "sh@rkpw1", "shark") 

cursor = conn.cursor()

cursor.execute("DROP TABLE IF EXISTS RoutineInfo")
cursor.execute("CREATE TABLE RoutineInfo(\
	id SMALLINT NOT NULL AUTO_INCREMENT,\
	info	longtext,\
	PRIMARY KEY (id)\
)")
cursor.execute("LOAD DATA LOCAL INFILE '/sandbox/salin/Documents/Python/Examples/MySQLdb/sptsv_29.txt' INTO TABLE RoutineInfo (info)")

cursor.execute("SELECT * FROM RoutineInfo")
print cursor.fetchall();


conn.commit()

cursor.close()
