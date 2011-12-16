import MySQLdb, csv

conn = MySQLdb.connect("hooray", "salin", "sh@rkpw1", "shark") 

cursor = conn.cursor()

cursor.execute("DROP TABLE IF EXISTS Linear_Equation")
cursor.execute("CREATE TABLE Linear_Equation(\
	id	SMALLINT AUTO_INCREMENT,\
	testcolumn	longtext,\
	PRIMARY KEY(id)\
)")


reader = csv.reader(open("Linear_Equation_txt.csv"))
for id, file in reader:
	fd = open("routineInfo/"+file, "r")
	conn.query("insert into Linear_Equation (testcolumn) values(\'%s\')" % (conn.escape_string(fd.read())))

cursor.execute("SELECT * FROM Linear_Equation")
results = cursor.fetchall()

#for item in results:
#	print item

print results

conn.close()
