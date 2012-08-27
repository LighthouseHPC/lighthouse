import csv
import os
parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
lighthousedir = os.path.dirname(os.path.dirname(parentdir))
os.sys.path.insert(0,parentdir) 


f_database = csv.reader(open(lighthousedir+'/Dlighthouse/Computational/dataBase/le_equilibrate.csv'))
databaseRoutines = []
for line in f_database:
    databaseRoutines.append(line[1]+line[2])
    
    
f_equilibrate_341 = open('./routines/equilibrate_341.txt')
equilibrate341Routines = []
equilibrate341Routines_equ = []
keys = []
for line in f_equilibrate_341:
    line = line.rstrip('\r\n')
    routine = line.split('.')[0]
    equilibrate341Routines.append(routine)
    last3 = routine[-3:]
    keys.append(last3)
    if last3 == 'equ':
        equilibrate341Routines_equ.append(routine)
    else:
        pass


equilibrate341Routines = set(equilibrate341Routines)

#print equilibrate341Routines

missingRoutines = list(set(equilibrate341Routines_equ) - set(databaseRoutines))

f_missing = open('./routines/missingRoutines.txt', 'w')

for routine in missingRoutines:
    f_missing.write(routine+'\n')
    

print "%s computational_equilibrate routines may need to be updated in the database." % len(missingRoutines)


### sort the routines in ./routines/equilibrate_341.txt
f_equilibrate_341_sort = open('./routines/equilibrate_341_sort.txt', 'w')
keys = set(keys)
print keys

for element in keys:
    i = 0
    for routine in equilibrate341Routines:
        if element == routine[-3:]:
            i += 1
            f_equilibrate_341_sort.write(routine+'\n')
        
    f_equilibrate_341_sort.write('-------------------------%s\r\n\n' % i)


f_equilibrate_341_sort.close()
f_equilibrate_341.close()
f_missing.close()