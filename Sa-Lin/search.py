import MySQLdb, csv, cgi, string


Drivers = MySQLdb.connect("hooray", "salin", "yellow34", "shark")
cursor = Drivers.cursor()

#List all the drivers tables.
Table_Names = ['Linear_Equation', 'Linear_Least_Squares', 'Sym_Eigen', 'nonSym_Eigen', 'SVD']

def search(word):
	if word == 'single':
		for item in Table_Names:
			print "--------------------", item, "--------------------"
			cursor.callproc("tableProc", [item, "T.thePrecision = 's'"])
			results = cursor.fetchall()
			for line in results:
				print line
			cursor.nextset()
			print

	elif word == 'double':
		for item in Table_Names:
			print "--------------------", item, "--------------------"
			cursor.callproc("tableProc", [item, "T.thePrecision = 'd'"])
			results = cursor.fetchall()
			for line in results:
				print line
			cursor.nextset()			

	elif word == 'complex':
		for item in Table_Names:
			print "--------------------", item, "--------------------"
			cursor.callproc("tableProc", [item, "T.thePrecision = 'c'"])
			results = cursor.fetchall()
			for line in results:
				print line
			cursor.nextset()
			print			

	elif word == 'complex 16' or word == 'double complex':
		for item in Table_Names:
			print "--------------------", item, "--------------------"
			cursor.callproc("tableProc", [item, "T.thePrecision = 'z'"])
			results = cursor.fetchall()
			for line in results:
				print line
			cursor.nextset()
			print		

	elif word == 'full':
		for item in Table_Names:
			print "--------------------", item, "--------------------"
			cursor.callproc("tableProc", [item, "T.structureType = 'f'"])
			results = cursor.fetchall()
			for line in results:
				print line
			cursor.nextset()
			print

	elif word == 'banded':
		for item in Table_Names:
			print "--------------------", item, "--------------------"
			cursor.callproc("tableProc", [item, "T.structureType = 'b'"])
			results = cursor.fetchall()
			for line in results:
				print line
			cursor.nextset()
			print

	elif word == 'packed':
		for item in Table_Names:
			print "--------------------", item, "--------------------"
			cursor.callproc("tableProc", [item, "T.structureType = 'p'"])
			results = cursor.fetchall()
			for line in results:
				print line
			cursor.nextset()
			print
	
	elif word == 'tridiagonal':
		for item in Table_Names:
			print "--------------------", item, "--------------------"
			cursor.callproc("tableProc", [item, "T.structureType = 't'"])
			results = cursor.fetchall()
			for line in results:
				print line
			cursor.nextset()
			print


	else:
		for item in Table_Names:
			print "--------------------", item, "--------------------"
			cursor.callproc("tableMatch", [item, word])
			results = cursor.fetchall()
			for line in results:
				print line
			cursor.nextset()
			print
			
		
search("general")

Drivers.commit()
Drivers.close()

