import csv
import sqlite3 as lite

myDB = lite.connect('/home/lily/lighthouse/sandbox/lily/django_orthg/orthg.db') 

cur = myDB.cursor()


reader = csv.reader(open("routine_info.csv"))

ID = raw_input('To load all data, enter "all"; otherwise, enter the ID number you wish to start loading:	')

print 'Your input is ' + ID


if ID == 'all':
	i = 1
else:
	i = int(ID)
	
for file in reader:
	fd = open("RoutineTxt/"+file[0], "r")
        try: 
       	    #myDB.execute("insert or ignore into orthg_lapack_routineinfo(id, routine, info) values(?,?,?)", (i, file[0], fd.read()))
            myDB.execute("UPDATE orthg_lapack_routineinfo SET routine = ? WHERE id = ?", (file[0], i))
        except lite.IntegrityError as err:
               print(err)
	i += 1     


myDB.commit()
cur.close()
myDB.close()

