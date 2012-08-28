import csv
import os
parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
lighthousedir = os.path.dirname(os.path.dirname(parentdir))
os.sys.path.insert(0,parentdir) 


f_database = csv.reader(open(lighthousedir+'/Dlighthouse/Driver/dataBase/le_simple.csv'))
databaseRoutines = []
for line in f_database:
    databaseRoutines.append(line[1]+line[2])
    
    
f_simple_341 = open('./routines/simple_341.txt')
simple341Routines = []
simple341Routines_trs = []
keys = []
for line in f_simple_341:
    line = line.rstrip('\r\n')
    routine = line.split('.')[0]
    simple341Routines.append(routine)
    last2 = routine[-2:]
    keys.append(last2)
    if last2 == 'sv':
        simple341Routines_trs.append(routine)
    else:
        pass


simple341Routines = set(simple341Routines)

#print simple341Routines

missingRoutines = list(set(simple341Routines_trs) - set(databaseRoutines))

f_missing = open('./routines/missingRoutines.txt', 'w')

for routine in missingRoutines:
    f_missing.write(routine+'\n')
    

print "%s computational_simple routines may need to be updated in the database." % len(missingRoutines)


### sort the routines in ./routines/simple_341.txt
f_simple_341_sort = open('./routines/simple_341_sort.txt', 'w')
keys = set(keys)
print keys

for element in keys:
    i = 0
    for routine in simple341Routines:
        if element == routine[-2:]:
            i += 1
            f_simple_341_sort.write(routine+'\n')
        
    f_simple_341_sort.write('-------------------------%s\r\n\n' % i)


f_simple_341_sort.close()
f_simple_341.close()
f_missing.close()