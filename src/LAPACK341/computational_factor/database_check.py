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
for line in f_factor_341:
    line = line.rstrip('\r\n')
    last3 = line.split('.')[0][-3:]
    #print last3
    if last3 == 'trf':
        factor341Routines.append(line.split('.')[0])
    else:
        pass
    

missingRoutines = list(set(factor341Routines) - set(databaseRoutines))


f_missing = open('./routines/missingRoutines.txt', 'w')

for routine in missingRoutines:
    f_missing.write(routine+'\n')
    

print "%s computational_factor routines may need to be updated in the database." % len(missingRoutines)

f_factor_341.close()
f_missing.close()