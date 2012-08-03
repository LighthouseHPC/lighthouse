import os, urllib, shutil, csv


print "------------ Make sure linear solvers routines are complete in the old version ------------"
###------------ compare linearSolve_old.txt (139) to le_driver_all.csv (82) + le_solve.csv (58)
# open e_driver_all.csv (82) + le_solve.csv (58) and put the routines in x
x = []
f_le_Driver_all = open('../../../Dlighthouse/Driver/le_driver_all.csv')
f_le_Computational_solve = open('../../../Dlighthouse/Computational/le_solve.csv')
for line in f_le_Driver_all:
    routine_file = line.split(",")[1]+line.split(",")[2]+".f"
    x.append(routine_file)
    #print routine_file

for line in f_le_Computational_solve:
    routine_file = line.split(",")[1]+line.split(",")[2]+".f"
    x.append(routine_file)
    #print routine_file

# open linearSolve_old.txt and put the routines in y
y = []
f_le_solve_old = open('routines/linearSolve_old.txt')
for routine in f_le_solve_old:
    y.append(routine[:-1])
    #print routine[:-1]

# find the routines that are in both routines_old_linearSolve.txt and le_driver_all.csv (82) + le_solve.csv (58)
#le_old_both = set(x) & set (y)
#print "The following %s routines are in both linearSolve_old.txt and lle_driver_all.csv (82) + le_solve.csv (58): "% len(le_old_both)
#for item in le_old_both:
#    print item

# find the routines that are in linearSolve_old.txt but NOT in le_driver_all.csv + le_solve.csv
missingRoutines = list(set(y) - set(x))
print "%s routines from linearSolve_old.txt are NOT in le_driver_all.csv + le_solve.csv:" % len(missingRoutines)
for item in missingRoutines:
    print item



# find the routines that are NOT in linearSolve_old.txt but in le_only.csv
missingRoutines2 = list(set(x) - set(y))
print "%s routines from le_driver_all.csv + le_solve.csv are NOT in linearSolve_old.txt:" % len(missingRoutines2)
for item in missingRoutines2:
    print item

print "------------ Compare to the v3.4.1 ------------"


###------------ Compare e_driver_all.csv (82) + le_solve.csv (58) to linearSolve_341.txt (198) for linear solvers ------------
###------------ find 'Linear Solve' routines that are in linearSolve_341.txt but NOT in le_driver_all.csv (82) + le_solve.csv (58) and
###------------ write them into routines/linearSolve_diff.txt
routines_solve_341 = []
routines_solve_diff = []
f_linearSolve_341 = open('routines/linearSolve_341.txt')
f_linearSolve_diff = open('routines/linearSolve_diff.txt', 'w')
for line in f_linearSolve_341:
    routineName= line.split(";")[-1].split("/")[-1].split(".f")[0]+'.f'
    routines_solve_341.append(routineName)
    if routineName not in x:
        routines_solve_diff.append(routineName)
        f_linearSolve_diff.write(routineName+'\n')
    else:
        pass

f_linearSolve_341.close()
f_linearSolve_diff.close()


