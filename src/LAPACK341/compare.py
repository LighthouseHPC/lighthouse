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
###------------ compare routines_old_linearSolve.txt (133) and le_only.csv (134)
# open le_only.csv and put the routines in x
x = []
f_le_solve_only = open('../../Dlighthouse/Combine/le_only.csv')
for line in f_le_solve_only:
    routine_file = line.split("/")[5].split(".f")[0]+'.f'
    x.append(routine_file)
    #print routine_file

# open routines_old_linearSolve.txt and put the routines in y
y = []
f_le_solve_old = open('routines/routines_old_linearSolve.txt')
for routine in f_le_solve_old:
    y.append(routine[:-1])
    #print routine[:-1]

# find the routines that are in both routines_old_linearSolve.txt and le_only.csv
#le_old_both = set(x) & set (y)
#print "The following %s routines are in both routines_old_linearSolve.txt and le_only.csv: "% len(le_old_both)
#for item in le_old_both:
#    print item

# find the routines that are in routines_old_linearSolve.txt but NOT in le_only.csv
missingRoutines = list(set(y) - set(x))
print "%s routines from routines_old_linearSolve.txt are NOT in le_only.csv:" % len(missingRoutines)
for item in missingRoutines:
    print item
print "================================================"


# find the routines that are NOT in routines_old_linearSolve.txt but in le_only.csv
missingRoutines2 = list(set(x) - set(y))
print "%s routines from le_only.csv are NOT in routines_old_linearSolve.txt:" % len(missingRoutines2)
for item in missingRoutines2:
    print item






###------------ Compare le_only.csv and routines_341_linearSolve.txt for linear solvers ------------
###------------ find 'Linear Solve' routines that are in routines_341_linearSolve.txt but NOT in le_only.csv and
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




####------------ fild the le routines that are neither in the old version (104) nor brand new (47)
## find all the linear solve routines in le_only.csv and put them in x
#x = []
#f_le_solve_only = open('../../Dlighthouse/Combine/le_only.csv')
#for line in f_le_solve_only:
#    routine_file = line.split("/")[5].split(".f")[0]+'.f'
#    x.append(routine_file)
#
####------------compare routines_solve_old to x, shoule be 125-104 = 21
#le_old_diff = list(set(routines_solve_old) - set(x))
#for item in le_old_diff:
#    print item
#
#
#y = list(set(routines_solve_341) - set(routines_solve_new))
#
#i=0
#for item in list(set(y) - set(x)):
#    i = i+1
##    print i, item
#
#print "================================================"


