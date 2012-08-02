import urllib, shutil, csv
import os
#parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
#lighthousedir = os.path.dirname(os.path.dirname(parentdir))
#os.sys.path.insert(0,parentdir) 


print "------------ Make sure 'solve' routines are complete in the old version ------------"
###------------ compare solve_old.txt to le_solve.csv
# open le_solve.csv and put the routines in x
x = []
f_le_Computational_solve = open('../../../Dlighthouse/Computational/le_solve.csv')
for line in f_le_Computational_solve:
    routine_file = line.split(",")[1]+line.split(",")[2]+".f"
    x.append(routine_file)
    #print routine_file

#print "x = ", x

# open solve_old.txt and put the routines in y
y = []
f_le_solve_old = open('./routines/solve_old.txt')
for routine in f_le_solve_old:
    y.append(routine[:-2])
    #print routine
    
#print "y = ", y

# find the routines that are in both solve_old.txt and le_solve.csv
#le_old_both = set(x) & set (y)
#print "The following %s routines are in both solve_old.txt and le_solve.csv: "% len(le_old_both)
#for item in le_old_both:
#    print item

# find the routines that are in solve_old.txt but NOT in le_solve.csv
missingRoutines = list(set(y) - set(x))
print "%s routines from solve_old.txt are NOT in le_solve.csv:" % len(missingRoutines)
for item in missingRoutines:
    print item
print "================================================"


# find the routines that are NOT in solve_old.txt but in le_solve.csv
missingRoutines2 = list(set(x) - set(y))
print "%s routines from le_solve.csv are NOT in solve_old.txt:" % len(missingRoutines2)
for item in missingRoutines2:
    print item


print "------------ Compare to the v3.4.1 ------------"


###------------ Compare le_solve.csv to solve_341.txt for linear solvers ------------
###------------ find 'solve' routines that are in solve_341.txt but NOT in le_solve.csv and
###------------ write them into routines/solve_diff.txt
routines_solve_341 = []
routines_solve_diff = []
f_solve_341 = open('./routines/solve_341.txt')
f_solve_diff = open('./routines/solve_diff.txt', 'w')
for line in f_solve_341:
    routineName= line.split(";")[-1].split("/")[-1].split(".f")[0]+'.f'
    routines_solve_341.append(routineName)
    if routineName not in x:
        routines_solve_diff.append(routineName)
        f_solve_diff.write(routineName+'\n')
    else:
        pass

f_solve_341.close()
f_solve_diff.close()

print "New 'solve' routines: %s." % len(routines_solve_diff)


