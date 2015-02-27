import sqlite3
import csv
import os
parentdir = os.path.abspath(os.path.join(os.path.dirname(__file__),"../../"))
#os.sys.path.insert(0,parentdir) 
data =[]

myDB = sqlite3.connect(database="/Users/lily/lighthouse-taxonomy_test/sandbox/lily/django_orthg/django_orthg/lighthouse_orthg.db") 

#cursor = myDB.cursor()



##### ----------------------------- IMPORTANT ---------------------------------#####
##### (1) ALL .csv files must be in the "Windows Comma Separated" format. #####
##### (2) MySQL storage engine must be set = MyISAM (old), not InnoDB (default) #####
##### -------------------------------------------------------------------------#####

#nr_records_inserted = cursor.execute("LOAD DATA LOCAL INFILE 'guided_orthg.csv' INTO TABLE \n lighthouse_lapack_orthg  FIELDS TERMINATED BY ',' LINES TERMINATED BY '\n'")

'''with open(parentdir+"/database/lapack_orthg/guided_orthg.csv", 'rt') as f:
     schema = f.read()
myDB.executescript(schema)'''

with open(parentdir+"/database/lapack_orthg/guided_orthg.csv", 'rU') as infh:
    rows = list(csv.reader(infh,delimiter=','))
    it=iter(rows)
    columns = zip(*it)
    next(columns, None)  # skip the first column

    with myDB as conn:
        cursor = conn.cursor()
        cursor.executemany('insert into  orthg_lapack_least_guided(id, thePrecision, routineName, standardGeneralized, complexNumber, FullStorage, sFullRank, gFullRank, svd, qr, singleDouble, notes, info_id) values(?,?,?,?,?,?,?,?,?,?,?,?,?)', columns)
i = 0
'''with open(parentdir+"/database/lapack_orthg/guided_orthg.csv") as f:
     data =[]
     for line in f:
            line = line.strip().split(",")
            data.append(x for x in line)
            print data[i]
            #columns = zip(data)
            with myDB as conn:
                 cursor = conn.cursor()
                 #cursor.executemany('insert into orthg_lapack_least_guided(id, thePrecision, routineName, standardGeneralized, complexNumber, FullStorage, sFullRank, gFullRank, svd, qr, singleDouble, notes, info_id) values(?,?,?,?,?,?,?,?,?,?,?,?,?)', data[i], data[i+1], data[i+2], data[i+3], data[i+4], data[i+5], data[i+6], data[i+7], data[i+8], data[i+9], data[i+10], data[i+11], data[i+12])'''
            
myDB.commit()
cursor.close()
myDB.close()

