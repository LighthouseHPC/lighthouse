import csv
import os
parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
lighthousedir = os.path.dirname(os.path.dirname(parentdir))
os.sys.path.insert(0,parentdir) 


f_database = csv.reader(open(lighthousedir+'/Dlighthouse/Computational/dataBase/le_condition_number.csv'))
databaseRoutines = []
for line in f_database:
    databaseRoutines.append(line[1]+line[2])
    
    
f_condNumber_341 = open('./routines/condNumber_341.txt')
condNumber341Routines = []
condNumber341Routines_con = []
keys = []
for line in f_condNumber_341:
    line = line.rstrip('\r\n')
    routine = line.split('.')[0]
    condNumber341Routines.append(routine)
    last3 = routine[-3:]
    keys.append(last3)
    if last3 == 'con':
        condNumber341Routines_con.append(routine)
    else:
        pass
    
condNumber341Routines = set(condNumber341Routines)

#print condNumber341Routines

missingRoutines = list(set(condNumber341Routines_con) - set(databaseRoutines))

f_missing = open('./routines/missingRoutines.txt', 'w')

for routine in missingRoutines:
    f_missing.write(routine+'\n')
    

print "%s computational_condNumber routines may need to be updated in the database." % len(missingRoutines)


### sort the routines in ./routines/condNumber_341.txt
f_condNumber_341_sort = open('./routines/condNumber_341_sort.txt', 'w')
keys = set(keys)
print keys

for element in keys:
    i = 0
    for routine in condNumber341Routines:
        if element == routine[-3:]:
            i += 1
            f_condNumber_341_sort.write(routine+'\n')
        
    f_condNumber_341_sort.write('-------------------------%s\r\n\n' % i)


f_condNumber_341_sort.close()
f_condNumber_341.close()
f_missing.close()