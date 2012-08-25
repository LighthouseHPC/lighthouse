import csv
import os
parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
lighthousedir = os.path.dirname(os.path.dirname(parentdir))
os.sys.path.insert(0,parentdir) 


f_database = csv.reader(open(lighthousedir+'/Dlighthouse/Computational/dataBase/le_factor.csv'))
databaseRoutines = []
for line in f_database:
    databaseRoutines.append(line[1]+line[2])
    
    
f_factor_341 = open('./routines/factor_341.txt')
factor341Routines = []
factor341Routines_trf = []
keys = []
for line in f_factor_341:
    line = line.rstrip('\r\n')
    routine = line.split('.')[0]
    factor341Routines.append(routine)
    last3 = routine[-3:]
    keys.append(last3)
    if last3 == 'trf':
        factor341Routines_trf.append(routine)
    else:
        pass
    

missingRoutines = list(set(factor341Routines_trf) - set(databaseRoutines))

f_missing = open('./routines/missingRoutines.txt', 'w')

for routine in missingRoutines:
    f_missing.write(routine+'\n')
    

print "%s computational_factor routines may need to be updated in the database." % len(missingRoutines)


### sort the routines in ./routines/factor_341.txt
f_factor_341_sort = open('./routines/factor_341_sort.txt', 'w')
keys = set(keys)
print keys

for element in keys:
    i = 0
    for routine in factor341Routines:
        if element == routine[-3:]:
            i += 1
            f_factor_341_sort.write(routine+'\n')
        
    f_factor_341_sort.write('-------------------------%s\r\n\n' % i)


f_factor_341_sort.close()
f_factor_341.close()
f_missing.close()