import urllib, shutil, csv
import os
#parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
#lighthousedir = os.path.dirname(os.path.dirname(parentdir))
#os.sys.path.insert(0,parentdir) 


print "------------ Make sure 'error Bound' routines are complete in the old version ------------"
###------------ compare inverse_old.txt to le_invert.csv
# open le_invert.csv and put the routines in x
x = []
f_le_Computational_inverse = open('../../../Dlighthouse/Computational/le_invert.csv')
for line in f_le_Computational_inverse:
    routine_file = line.split(",")[1]+line.split(",")[2]+".f"
    x.append(routine_file)
    #print routine_file

#print "x = ", x

# open inverse_old.txt and put the routines in y
y = []
f_le_solve_old = open('./routines/inverse_old.txt')
for routine in f_le_solve_old:
    y.append(routine[:-2])
    #print routine
    
#print "y = ", y

# find the routines that are in both inverse_old.txt and le_invert.csv
#le_old_both = set(x) & set (y)
#print "The following %s routines are in both inverse_old.txt and le_invert.csv: "% len(le_old_both)
#for item in le_old_both:
#    print item

# find the routines that are in inverse_old.txt but NOT in le_invert.csv
missingRoutines = list(set(y) - set(x))
print "%s routines from inverse_old.txt are NOT in le_invert.csv:" % len(missingRoutines)
for item in missingRoutines:
    print item
print "================================================"


# find the routines that are NOT in inverse_old.txt but in le_invert.csv
missingRoutines2 = list(set(x) - set(y))
print "%s routines from le_invert.csv are NOT in inverse_old.txt:" % len(missingRoutines2)
for item in missingRoutines2:
    print item


print "------------ Compare to the v3.4.1 ------------"


###------------ Compare le_invert.csv to inverse_341.txt for linear solvers ------------
###------------ find 'inverse' routines that are in inverse_341.txt but NOT in le_invert.csv and
###------------ write them into routines/inverse_diff.txt
routines_inverse_341 = []
routines_inverse_diff = []
f_inverse_341 = open('./routines/inverse_341.txt')
f_inverse_diff = open('./routines/inverse_diff.txt', 'w')
for line in f_inverse_341:
    routineName= line.split(";")[-1].split("/")[-1].split(".f")[0]+'.f'
    routines_inverse_341.append(routineName)
    if routineName not in x:
        routines_inverse_diff.append(routineName)
        f_inverse_diff.write(routineName+'\n')
    else:
        pass

f_inverse_341.close()
f_inverse_diff.close()

print "New 'inverse' routines: %s." % len(routines_inverse_diff)


