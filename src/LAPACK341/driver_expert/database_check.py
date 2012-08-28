import csv
import os
parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
lighthousedir = os.path.dirname(os.path.dirname(parentdir))
os.sys.path.insert(0,parentdir) 


f_database = csv.reader(open(lighthousedir+'/Dlighthouse/Driver/dataBase/le_expert.csv'))
databaseRoutines = []
for line in f_database:
    databaseRoutines.append(line[1]+line[2])
    
    
f_expert_341 = open('./routines/expert_341.txt')
expert341Routines = []
expert341Routines_trs = []
keys = []
for line in f_expert_341:
    line = line.rstrip('\r\n')
    routine = line.split('.')[0]
    expert341Routines.append(routine)
    last2 = routine[-2:]
    keys.append(last2)
    if last2 == 'vx':
        expert341Routines_trs.append(routine)
    else:
        pass


expert341Routines = set(expert341Routines)

#print expert341Routines

missingRoutines = list(set(expert341Routines_trs) - set(databaseRoutines))

f_missing = open('./routines/missingRoutines.txt', 'w')

for routine in missingRoutines:
    f_missing.write(routine+'\n')
    

print "%s computational_expert routines may need to be updated in the database." % len(missingRoutines)


### sort the routines in ./routines/expert_341.txt
f_expert_341_sort = open('./routines/expert_341_sort.txt', 'w')
keys = set(keys)
print keys

for element in keys:
    i = 0
    for routine in expert341Routines:
        if element == routine[-2:]:
            i += 1
            f_expert_341_sort.write(routine+'\n')
        
    f_expert_341_sort.write('-------------------------%s\r\n\n' % i)


f_expert_341_sort.close()
f_expert_341.close()
f_missing.close()