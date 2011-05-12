import MySQLdb, csv

conn = MySQLdb.connect("hooray", "salin", "yellow34", "shark") 

cursor = conn.cursor()


reader = csv.reader(open("routineList.csv"))

for idn, precision, routine, url, file_name in reader:
	tableName = str(precision+routine+"_Table"+idn)
	print tableName
	cursor.execute("DROP TABLE IF EXISTS %s" % tableName)
	cursor.execute("CREATE TABLE %s (content	MEDIUMTEXT, FULLTEXT(content))" % tableName)
	fd = open(file_name, "r")
	cursor.execute("LOAD DATA LOCAL INFILE '%s' INTO TABLE %s" % (file_name, tableName))


conn.close()
