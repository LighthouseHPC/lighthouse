import csv
import os
parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
lighthousedir = os.path.dirname(os.path.dirname(parentdir))
os.sys.path.insert(0,parentdir) 


f_database = csv.reader(open(lighthousedir+'/Dlighthouse/Computational/dataBase/le_error_bounds.csv'))
databaseRoutines = []
for line in f_database:
    databaseRoutines.append(line[1]+line[2])
    
    
f_errorBound_341 = open('./routines/errorBound_341.txt')
errorBound341Routines = []
errorBound341Routines_rfs = []
keys = []
for line in f_errorBound_341:
    line = line.rstrip('\r\n')
    routine = line.split('.')[0]
    errorBound341Routines.append(routine)
    last3 = routine[-3:]
    keys.append(last3)
    if last3 == 'rfs':
        errorBound341Routines_rfs.append(routine)
    else:
        pass


errorBound341Routines = set(errorBound341Routines)

#print errorBound341Routines

missingRoutines = list(set(errorBound341Routines_rfs) - set(databaseRoutines))

f_missing = open('./routines/missingRoutines.txt', 'w')

for routine in missingRoutines:
    f_missing.write(routine+'\n')
    

print "%s computational_errorBound routines may need to be updated in the database." % len(missingRoutines)


### sort the routines in ./routines/errorBound_341.txt
f_errorBound_341_sort = open('./routines/errorBound_341_sort.txt', 'w')
keys = set(keys)
print keys

for element in keys:
    i = 0
    for routine in errorBound341Routines:
        if element == routine[-3:]:
            i += 1
            f_errorBound_341_sort.write(routine+'\n')
        
    f_errorBound_341_sort.write('-------------------------%s\r\n\n' % i)


f_errorBound_341_sort.close()
f_errorBound_341.close()
f_missing.close()