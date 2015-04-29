import csv
import sqlite3 as lite

myDB = lite.connect('/home/lily/lighthouse/sandbox/lily/lily_4_10/django_orthg/orthg.db') 

cur = myDB.cursor()


reader = csv.reader(open("guided_least.csv"))

ID = raw_input('To load all data, enter "all"; otherwise, enter the ID number you wish to start loading:	')

print 'Your input is ' + ID


if ID == 'all':
	i = 1
else:
	i = int(ID)
	
for row in reader:
       	    #myDB.execute("insert or ignore into orthg_lapack_routineinfo(id, routine, info) values(?,?,?)", (i, file[0], fd.read()))
            myDB.execute("update into orthg_least(id,thePrecision,routineName,standardGeneralized,complexNumber,FullStorage,sFullRank,gFullRank,svd,qr,singleDouble,notes,info) values(?,?,?,?,?,?,?,?,?,?,?,?,?)", (i, f[1],f[2],f[3],f[4],f[5],f[6],f[7],f[8],f[9],f[10],f[11],f[12]))
       	    i += 1     


myDB.commit()
cur.close()
myDB.close()

