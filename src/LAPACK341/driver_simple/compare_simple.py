import urllib, shutil, csv
import os
#parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
#lighthousedir = os.path.dirname(os.path.dirname(parentdir))
#os.sys.path.insert(0,parentdir) 


print "------------ Make sure 'simple' routines are complete in the old version ------------"
###------------ compare simple_old.txt to le_simple.csv
# open le_simple.csv and put the routines in x
x = []
f_le_solve_simple = open('../../../Dlighthouse/Driver/le_simple.csv')
for line in f_le_solve_simple:
    routine_file = line.split(",")[1]+line.split(",")[2]+".f"
    x.append(routine_file)
    #print routine_file

#print "x = ", x

# open simple_old.txt and put the routines in y
y = []
f_le_solve_old = open('./routines/simple_old.txt')
for routine in f_le_solve_old:
    y.append(routine[:-2])
    #print routine
    
#print "y = ", y

# find the routines that are in both simple_old.txt and le_simple.csv
#le_old_both = set(x) & set (y)
#print "The following %s routines are in both simple_old.txt and le_simple.csv: "% len(le_old_both)
#for item in le_old_both:
#    print item

# find the routines that are in simple_old.txt but NOT in le_simple.csv
missingRoutines = list(set(y) - set(x))
print "%s routines from simple_old.txt are NOT in le_simple.csv:" % len(missingRoutines)
for item in missingRoutines:
    print item
print "================================================"


# find the routines that are NOT in simple_old.txt but in le_simple.csv
missingRoutines2 = list(set(x) - set(y))
print "%s routines from le_simple.csv are NOT in simple_old.txt:" % len(missingRoutines2)
for item in missingRoutines2:
    print item


print "------------ Compare to the v3.4.1 ------------"


###------------ Compare le_simple.csv to simple_341.txt for linear solvers ------------
###------------ find 'simple' routines that are in simple_341.txt but NOT in le_simple.csv and
###------------ write them into routines/simple_diff.txt
routines_simple_341 = []
routines_simple_diff = []
f_simple_341 = open('./routines/simple_341.txt')
f_simple_diff = open('./routines/simple_diff.txt', 'w')
for line in f_simple_341:
    routineName= line.split(";")[-1].split("/")[-1].split(".f")[0]+'.f'
    routines_simple_341.append(routineName)
    if routineName not in x:
        routines_simple_diff.append(routineName)
        f_simple_diff.write(routineName+'\n')
    else:
        pass

f_simple_341.close()
f_simple_diff.close()

print "New 'simple driver' routines: %s." % len(routines_simple_diff)


