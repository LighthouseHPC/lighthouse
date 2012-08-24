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
for line in f_solve_341:
    solve341Routines.append(line.rstrip('.f\r\n'))
    

missingRoutines = list(set(solve341Routines) - set(databaseRoutines))


f_missing = open('./routines/missingRoutines.txt', 'w')

for routine in missingRoutines:
    f_missing.write(routine+'\n')
    

print "%s computational_solve routines may need to be updated in the database." % len(missingRoutines)

f_solve_341.close()
f_missing.close()