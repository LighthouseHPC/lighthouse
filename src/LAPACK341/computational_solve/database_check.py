import csv
import os
parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
lighthousedir = os.path.dirname(os.path.dirname(parentdir))
os.sys.path.insert(0,parentdir) 


f_database = csv.reader(open(lighthousedir+'/Dlighthouse/Computational/dataBase/le_solve.csv'))
databaseRoutines = []
for line in f_database:
    databaseRoutines.append(line[1]+line[2])
    
    
f_solve_341 = open('./routines/solve_341.txt')
solve341Routines = []
solve341Routines_trs = []
keys = []
for line in f_solve_341:
    line = line.rstrip('\r\n')
    routine = line.split('.')[0]
    solve341Routines.append(routine)
    last3 = routine[-3:]
    keys.append(last3)
    if last3 == 'trs':
        solve341Routines_trs.append(routine)
    else:
        pass


solve341Routines = set(solve341Routines)

#print solve341Routines

missingRoutines = list(set(solve341Routines_trs) - set(databaseRoutines))

f_missing = open('./routines/missingRoutines.txt', 'w')

for routine in missingRoutines:
    f_missing.write(routine+'\n')
    

print "%s computational_solve routines may need to be updated in the database." % len(missingRoutines)


### sort the routines in ./routines/solve_341.txt
f_solve_341_sort = open('./routines/solve_341_sort.txt', 'w')
keys = set(keys)
print keys

for element in keys:
    i = 0
    for routine in solve341Routines:
        if element == routine[-3:]:
            i += 1
            f_solve_341_sort.write(routine+'\n')
        
    f_solve_341_sort.write('-------------------------%s\r\n\n' % i)


f_solve_341_sort.close()
f_solve_341.close()
f_missing.close()