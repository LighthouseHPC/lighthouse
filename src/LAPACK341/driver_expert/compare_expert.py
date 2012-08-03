import urllib, shutil, csv
import os
#parentdir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
#lighthousedir = os.path.dirname(os.path.dirname(parentdir))
#os.sys.path.insert(0,parentdir) 


print "------------ Make sure 'expert' routines are complete in the old version ------------"
###------------ compare expert_old.txt to le_expert.csv
# open le_expert.csv and put the routines in x
x = []
f_le_solve_expert = open('../../../Dlighthouse/Driver/le_expert.csv')
for line in f_le_solve_expert:
    routine_file = line.split(",")[1]+line.split(",")[2]+".f"
    x.append(routine_file)
    #print routine_file

#print "x = ", x

# open expert_old.txt and put the routines in y
y = []
f_le_solve_old = open('./routines/expert_old.txt')
for routine in f_le_solve_old:
    y.append(routine[:-2])
    #print routine
    
#print "y = ", y

# find the routines that are in both expert_old.txt and le_expert.csv
#le_old_both = set(x) & set (y)
#print "The following %s routines are in both expert_old.txt and le_expert.csv: "% len(le_old_both)
#for item in le_old_both:
#    print item

# find the routines that are in expert_old.txt but NOT in le_expert.csv
missingRoutines = list(set(y) - set(x))
print "%s routines from expert_old.txt are NOT in le_expert.csv:" % len(missingRoutines)
for item in missingRoutines:
    print item
print "================================================"


# find the routines that are NOT in expert_old.txt but in le_expert.csv
missingRoutines2 = list(set(x) - set(y))
print "%s routines from le_expert.csv are NOT in expert_old.txt:" % len(missingRoutines2)
for item in missingRoutines2:
    print item


print "------------ Compare to the v3.4.1 ------------"


###------------ Compare le_expert.csv to expert_341.txt for linear solvers ------------
###------------ find 'expert' routines that are in expert_341.txt but NOT in le_expert.csv and
###------------ write them into routines/expert_diff.txt
routines_expert_341 = []
routines_expert_diff = []
f_expert_341 = open('./routines/expert_341.txt')
f_expert_diff = open('./routines/expert_diff.txt', 'w')
for line in f_expert_341:
    routineName= line.split(";")[-1].split("/")[-1].split(".f")[0]+'.f'
    routines_expert_341.append(routineName)
    if routineName not in x:
        routines_expert_diff.append(routineName)
        f_expert_diff.write(routineName+'\n')
    else:
        pass

f_expert_341.close()
f_expert_diff.close()

print "New 'expert driver' routines: %s." % len(routines_expert_diff)


