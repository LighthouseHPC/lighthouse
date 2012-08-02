import urllib, shutil, csv
import os
#parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
#lighthousedir = os.path.dirname(os.path.dirname(parentdir))
#os.sys.path.insert(0,parentdir) 


print "------------ Make sure 'equilibrate' routines are complete in the old version ------------"
###------------ compare equilibrate_old.txt to le_equilibrate.csv
# open le_equilibrate.csv and put the routines in x
x = []
f_le_Computational_equilibrate = open('../../../Dlighthouse/Computational/le_equilibrate.csv')
for line in f_le_Computational_equilibrate:
    routine_file = line.split(",")[1]+line.split(",")[2]+".f"
    x.append(routine_file)
    #print routine_file

#print "x = ", x

# open equilibrate_old.txt and put the routines in y
y = []
f_le_solve_old = open('./routines/equilibrate_old.txt')
for routine in f_le_solve_old:
    y.append(routine[:-2])
    #print routine
    
#print "y = ", y

# find the routines that are in both equilibrate_old.txt and le_equilibrate.csv
#le_old_both = set(x) & set (y)
#print "The following %s routines are in both equilibrate_old.txt and le_equilibrate.csv: "% len(le_old_both)
#for item in le_old_both:
#    print item

# find the routines that are in equilibrate_old.txt but NOT in le_equilibrate.csv
missingRoutines = list(set(y) - set(x))
print "%s routines from equilibrate_old.txt are NOT in le_equilibrate.csv:" % len(missingRoutines)
for item in missingRoutines:
    print item
print "================================================"


# find the routines that are NOT in equilibrate_old.txt but in le_equilibrate.csv
missingRoutines2 = list(set(x) - set(y))
print "%s routines from le_equilibrate.csv are NOT in equilibrate_old.txt:" % len(missingRoutines2)
for item in missingRoutines2:
    print item


print "------------ Compare to the v3.4.1 ------------"


###------------ Compare le_equilibrate.csv to equilibrate_341.txt for linear solvers ------------
###------------ find 'equilibrate' routines that are in equilibrate_341.txt but NOT in le_equilibrate.csv and
###------------ write them into routines/equilibrate_diff.txt
routines_equilibrate_341 = []
routines_equilibrate_diff = []
f_equilibrate_341 = open('./routines/equilibrate_341.txt')
f_equilibrate_diff = open('./routines/equilibrate_diff.txt', 'w')
for line in f_equilibrate_341:
    routineName= line.split(";")[-1].split("/")[-1].split(".f")[0]+'.f'
    routines_equilibrate_341.append(routineName)
    if routineName not in x:
        routines_equilibrate_diff.append(routineName)
        f_equilibrate_diff.write(routineName+'\n')
    else:
        pass

f_equilibrate_341.close()
f_equilibrate_diff.close()

print "New 'equilibrate' routines: %s." % len(routines_equilibrate_diff)


