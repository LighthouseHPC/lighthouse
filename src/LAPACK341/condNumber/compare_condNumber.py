import urllib, shutil, csv
import os
#parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
#lighthousedir = os.path.dirname(os.path.dirname(parentdir))
#os.sys.path.insert(0,parentdir) 


print "------------ Make sure 'condNumber' routines are complete in the old version ------------"
###------------ compare condNumber_old.txt to le_condition_number.csv
# open le_condition_number.csv and put the routines in x
x = []
f_le_Computational_condNumber = open('../../../Dlighthouse/Computational/le_condition_number.csv')
for line in f_le_Computational_condNumber:
    routine_file = line.split(",")[1]+line.split(",")[2]+".f"
    x.append(routine_file)
    #print routine_file

#print "x = ", x

# open condNumber_old.txt and put the routines in y
y = []
f_le_solve_old = open('./routines/condNumber_old.txt')
for routine in f_le_solve_old:
    y.append(routine[:-2])
    #print routine
    
#print "y = ", y

# find the routines that are in both condNumber_old.txt and le_condition_number.csv
#le_old_both = set(x) & set (y)
#print "The following %s routines are in both condNumber_old.txt and le_condition_number.csv: "% len(le_old_both)
#for item in le_old_both:
#    print item

# find the routines that are in condNumber_old.txt but NOT in le_condition_number.csv
missingRoutines = list(set(y) - set(x))
print "%s routines from condNumber_old.txt are NOT in le_condition_number.csv:" % len(missingRoutines)
for item in missingRoutines:
    print item
print "================================================"


# find the routines that are NOT in condNumber_old.txt but in le_condition_number.csv
missingRoutines2 = list(set(x) - set(y))
print "%s routines from le_condition_number.csv are NOT in condNumber_old.txt:" % len(missingRoutines2)
for item in missingRoutines2:
    print item


print "------------ Compare to the v3.4.1 ------------"


###------------ Compare le_condition_number.csv to condNumber_341.txt for linear solvers ------------
###------------ find 'condNumber' routines that are in condNumber_341.txt but NOT in le_condition_number.csv and
###------------ write them into routines/condNumber_diff.txt
routines_condNumber_341 = []
routines_condNumber_diff = []
f_condNumber_341 = open('./routines/condNumber_341.txt')
f_condNumber_diff = open('./routines/condNumber_diff.txt', 'w')
for line in f_condNumber_341:
    routineName= line.split(";")[-1].split("/")[-1].split(".f")[0]+'.f'
    routines_condNumber_341.append(routineName)
    if routineName not in x:
        routines_condNumber_diff.append(routineName)
        f_condNumber_diff.write(routineName+'\n')
    else:
        pass

f_condNumber_341.close()
f_condNumber_diff.close()

print "New 'condNumber' routines: %s." % len(routines_condNumber_diff)


