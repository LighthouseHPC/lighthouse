import urllib, shutil, csv
import os
#parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
#lighthousedir = os.path.dirname(os.path.dirname(parentdir))
#os.sys.path.insert(0,parentdir) 


print "------------ Make sure 'error Bound' routines are complete in the old version ------------"
###------------ compare errorBound_old.txt to le_error_bounds.csv
# open le_error_bounds.csv and put the routines in x
x = []
f_le_Computational_errorBound = open('../../../Dlighthouse/Computational/dataBase/le_error_bounds.csv')
for line in f_le_Computational_errorBound:
    routine_file = line.split(",")[1]+line.split(",")[2]+".f"
    x.append(routine_file)
    #print routine_file

#print "x = ", x

# open errorBound_old.txt and put the routines in y
y = []
f_le_solve_old = open('./routines/errorBound_old.txt')
for routine in f_le_solve_old:
    y.append(routine[:-2])
    #print routine
    
#print "y = ", y

# find the routines that are in both errorBound_old.txt and le_error_bounds.csv
#le_old_both = set(x) & set (y)
#print "The following %s routines are in both errorBound_old.txt and le_error_bounds.csv: "% len(le_old_both)
#for item in le_old_both:
#    print item

# find the routines that are in errorBound_old.txt but NOT in le_error_bounds.csv
missingRoutines = list(set(y) - set(x))
print "%s routines from errorBound_old.txt are NOT in le_error_bounds.csv:" % len(missingRoutines)
for item in missingRoutines:
    print item
print "================================================"


# find the routines that are NOT in errorBound_old.txt but in le_error_bounds.csv
missingRoutines2 = list(set(x) - set(y))
print "%s routines from le_error_bounds.csv are NOT in errorBound_old.txt:" % len(missingRoutines2)
for item in missingRoutines2:
    print item


print "------------ Compare to the v3.4.1 ------------"


###------------ Compare le_error_bounds.csv to errorBound_341.txt for linear solvers ------------
###------------ find 'errorBound' routines that are in errorBound_341.txt but NOT in le_error_bounds.csv and
###------------ write them into routines/errorBound_diff.txt
routines_errorBound_341 = []
routines_errorBound_diff = []
f_errorBound_341 = open('./routines/errorBound_341.txt')
f_errorBound_diff = open('./routines/errorBound_diff.txt', 'w')
for line in f_errorBound_341:
    routineName= line.split(";")[-1].split("/")[-1].split(".f")[0]+'.f'
    routines_errorBound_341.append(routineName)
    if routineName not in x:
        routines_errorBound_diff.append(routineName)
        f_errorBound_diff.write(routineName+'\n')
    else:
        pass

f_errorBound_341.close()
f_errorBound_diff.close()

print "New 'errorBound' routines: %s." % len(routines_errorBound_diff)


