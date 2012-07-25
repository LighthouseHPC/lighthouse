import os, urllib, shutil, csv


###------------ make a list of the routines from the old version
def Old_List():
    old_list = []
    f_single = open("single_old.html")
    f_double = open("double_old.html")
    f_complex = open("complex_old.html")
    f_complex16 = open("complex16_old.html")

    for line in f_single:
        if "href=" in line:
            old_list.append(line.split('\"')[1])
        else:
            pass
       
    for line in f_double:
        if "href=" in line:
            old_list.append(line.split('\"')[1])
        else:
            pass
    
    for line in f_complex:
        if "href=" in line:
            old_list.append(line.split('\"')[1])
        else:
            pass
    
    for line in f_complex16:
        if "href=" in line:
            old_list.append(line.split('\"')[1])
        else:
            pass
    
    f_single.close()
    f_double.close()
    f_complex.close()
    f_complex16.close()
    return old_list



old_list = Old_List()
print "Old verion: %s routines." % len(old_list)





###------------ check for duplicate elements in old_list
dups = [x for x in old_list if old_list.count(x) > 1]
print "Duplicate elements in the old version:"
for item in dups:
    print item
print "================================================"





###------------ make a list of the routines from the new (3.4.1) version
def New_List():    
    new_list = []
    f_341 = open("routines_341.html")
    for line in f_341:
        if "href" in line:
            new_list.append(line.split('\"')[1])
        else:
            pass
        
    f_341.close()
    return new_list

new_list = New_List()
print "Version 3.4.1: %s routines." % len(new_list)



###------------ find the routines that are in both versions
both = set(old_list) & set(new_list)
print "In both versions: %s routines." % len(both)




###------------ find the routines that are not in the old version
routines_new = list(set(new_list) - set(old_list))
print "New in v3.4.1: %s routines." % len(routines_new)
print "================================================"



###------------ find the rouitnes that are in the old but not in the new verion
trashed = list(set(old_list) - set(both))
print "The following old routines are not in version 3.4.1:"
for item in trashed:
    print item






print "------------ Make sure linear solvers routines are complete in the old version ------------"
###------------ compare routines_old_linearSolve.txt (139) to le_driver_all.csv (82) + le_solve.csv (58)
# open e_driver_all.csv (82) + le_solve.csv (58) and put the routines in x
x = []
f_le_Driver_all = open('../../Dlighthouse/Driver/le_driver_all.csv')
f_le_Computational_solve = open('../../Dlighthouse/Computational/le_solve.csv')
for line in f_le_Driver_all:
    routine_file = line.split(",")[1]+line.split(",")[2]+".f"
    x.append(routine_file)
    #print routine_file

for line in f_le_Computational_solve:
    routine_file = line.split(",")[1]+line.split(",")[2]+".f"
    x.append(routine_file)
    #print routine_file

# open routines_old_linearSolve.txt and put the routines in y
y = []
f_le_solve_old = open('routines/routines_old_linearSolve.txt')
for routine in f_le_solve_old:
    y.append(routine[:-1])
    #print routine[:-1]

# find the routines that are in both routines_old_linearSolve.txt and le_driver_all.csv (82) + le_solve.csv (58)
#le_old_both = set(x) & set (y)
#print "The following %s routines are in both routines_old_linearSolve.txt and lle_driver_all.csv (82) + le_solve.csv (58): "% len(le_old_both)
#for item in le_old_both:
#    print item

# find the routines that are in routines_old_linearSolve.txt but NOT in le_only.csv
missingRoutines = list(set(y) - set(x))
print "%s routines from routines_old_linearSolve.txt are NOT in le_driver_all.csv + le_solve.csv:" % len(missingRoutines)
for item in missingRoutines:
    print item
print "================================================"


# find the routines that are NOT in routines_old_linearSolve.txt but in le_only.csv
missingRoutines2 = list(set(x) - set(y))
print "%s routines from le_driver_all.csv + le_solve.csv are NOT in routines_old_linearSolve.txt:" % len(missingRoutines2)
for item in missingRoutines2:
    print item




###------------ Compare e_driver_all.csv (82) + le_solve.csv (58) to routines_341_linearSolve.txt (198) for linear solvers ------------
###------------ find 'Linear Solve' routines that are in routines_341_linearSolve.txt but NOT in e_driver_all.csv (82) + le_solve.csv (58) and
###------------ write them into routines/routines_341_linearSolve_new.txt
routines_solve_341 = []
routines_solve_diff = []
f_linearSolve_341 = open('routines/routines_341_linearSolve.txt')
f_linearSolve_diff = open('routines/routines_341_linearSolve_new.txt', 'w')
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




