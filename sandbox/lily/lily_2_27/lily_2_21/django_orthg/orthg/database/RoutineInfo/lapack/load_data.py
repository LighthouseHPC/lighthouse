import csv
import sqlite3
import codecs

def quote_identifier(s, errors="strict"):
    encodable = s.encode("utf-8", errors).decode("utf-8")

    nul_index = encodable.find("\x00")

    if nul_index >= 0:
        error = UnicodeEncodeError("NUL-terminated utf-8", encodable,
                                   nul_index, nul_index + 1, "NUL not allowed")
        error_handler = codecs.lookup_error(errors)
        replacement, _ = error_handler(error)
        encodable = encodable.replace("\x00", replacement)

    return "\"" + encodable.replace("\"", "\"\"") + "\""

myDB = sqlite3.connect(database="/Users/lily/lighthouse-taxonomy_test/sandbox/lily/django_orthg/django_orthg/lighthouse_orthg.db") 

cursor = myDB.cursor()


reader = csv.reader(open("routine_info.csv"))

ID = raw_input('To load all data, enter "all"; otherwise, enter the ID number you wish to start loading:	')

print 'Your input is ' + ID


if ID == 'all':
	i = 1
else:
	i = int(ID)

data = []
with open("routine_info.csv") as reader:
     reader.readline()
      #cursor.executemany("update orthg_lapack_routineinfo set id=?", ((str(i),)for i in reader)
     for line in reader:
         line = line.strip().split(",")
         #data.append(x for x in line)
         #print line
         cells = [
         (i,str(line)),
         ]
         cursor.executemany("update orthg_lapack_routineinfo set id=?", (str(i),))
         i += 1
	
'''for file in reader:
	fd = open("RoutineTxt/"+file[0], "r")
        data = []
        #fd = fd.strip().split(",")
        data.append(x for x in fd)

	cursor.executemany("insert into orthg_lapack_routineinfo (id, routine) values (?,?)", str(i), file[0])
	i += 1'''


myDB.commit()
cursor.close()
myDB.close()

