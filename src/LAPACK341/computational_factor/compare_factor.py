import urllib, shutil, csv
import os
#parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
#lighthousedir = os.path.dirname(os.path.dirname(parentdir))
#os.sys.path.insert(0,parentdir) 


print "------------ Make sure 'factor' routines are complete in the old version ------------"
###------------ compare factor_old.txt to le_factor.csv
# open le_factor.csv and put the routines in x
x = []
f_le_Computational_factor = open('../../../Dlighthouse/Computational/le_factor.csv')
for line in f_le_Computational_factor:
    routine_file = line.split(",")[1]+line.split(",")[2]+".f"
    x.append(routine_file)
    #print routine_file

#print "x = ", x

# open factor_old.txt and put the routines in y
y = []
f_le_solve_old = open('./routines/factor_old.txt')
for routine in f_le_solve_old:
    y.append(routine[:-2])
    #print routine
    
#print "y = ", y

# find the routines that are in both factor_old.txt and le_factor.csv
#le_old_both = set(x) & set (y)
#print "The following %s routines are in both factor_old.txt and le_factor.csv: "% len(le_old_both)
#for item in le_old_both:
#    print item

# find the routines that are in factor_old.txt but NOT in le_factor.csv
missingRoutines = list(set(y) - set(x))
print "%s routines from factor_old.txt are NOT in le_factor.csv:" % len(missingRoutines)
for item in missingRoutines:
    print item
print "================================================"


# find the routines that are NOT in factor_old.txt but in le_factor.csv
missingRoutines2 = list(set(x) - set(y))
print "%s routines from le_factor.csv are NOT in factor_old.txt:" % len(missingRoutines2)
for item in missingRoutines2:
    print item


print "------------ Compare to the v3.4.1 ------------"


###------------ Compare le_factor.csv to factor_341.txt for linear solvers ------------
###------------ find 'factor' routines that are in factor_341.txt but NOT in le_factor.csv and
###------------ write them into routines/factor_diff.txt
routines_factor_341 = []
routines_factor_diff = []
f_factor_341 = open('./routines/factor_341.txt')
f_factor_diff = open('./routines/factor_diff.txt', 'w')
for line in f_factor_341:
    routineName= line.split(";")[-1].split("/")[-1].split(".f")[0]+'.f'
    routines_factor_341.append(routineName)
    if routineName not in x:
        routines_factor_diff.append(routineName)
        f_factor_diff.write(routineName+'\n')
    else:
        pass

f_factor_341.close()
f_factor_diff.close()

print "New 'factor' routines: %s." % len(routines_factor_diff)


