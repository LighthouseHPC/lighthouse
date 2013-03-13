import csv
import os
parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
lighthousedir = os.path.dirname(os.path.dirname(parentdir))
os.sys.path.insert(0,parentdir) 


f_database = csv.reader(open(lighthousedir+'/Dlighthouse/Computational/dataBase/le_invert.csv'))
databaseRoutines = []
for line in f_database:
    databaseRoutines.append(line[1]+line[2])
    
    
f_inverse_341 = open('./routines/inverse_341.txt')
inverse341Routines = []
inverse341Routines_tri = []
keys = []
for line in f_inverse_341:
    line = line.rstrip('\r\n')
    routine = line.split('.')[0]
    inverse341Routines.append(routine)
    last3 = routine[-3:]
    keys.append(last3)
    if last3 == 'tri':
        inverse341Routines_tri.append(routine)
    else:
        pass


inverse341Routines = set(inverse341Routines)

#print inverse341Routines

missingRoutines = list(set(inverse341Routines_tri) - set(databaseRoutines))

f_missing = open('./routines/missingRoutines.txt', 'w')

for routine in missingRoutines:
    f_missing.write(routine+'\n')
    

print "%s computational_inverse routines may need to be updated in the database." % len(missingRoutines)


### sort the routines in ./routines/inverse_341.txt
f_inverse_341_sort = open('./routines/inverse_341_sort.txt', 'w')
keys = set(keys)
print keys

for element in keys:
    i = 0
    for routine in inverse341Routines:
        if element == routine[-3:]:
            i += 1
            f_inverse_341_sort.write(routine+'\n')
        
    f_inverse_341_sort.write('-------------------------%s\r\n\n' % i)


f_inverse_341_sort.close()
f_inverse_341.close()
f_missing.close()