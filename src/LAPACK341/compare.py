import os, urllib, shutil

f_single = open("single_old.html")
f_double = open("double_old.html")
f_complex = open("complex_old.html")
f_complex16 = open("complex16_old.html")

f_341 = open("routines_341.html")


###------------ make a list of the routines from the old version
old_list = []
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

print "There are %s routines in the old version." % len(old_list)
f_single.close()
f_double.close()
f_complex.close()
f_complex16.close()



###------------ make a list of the routines from the new (3.4.1) version
new_list = []
for line in f_341:
    if "href" in line:
        new_list.append(line.split('\"')[1])
    else:
        pass

print "There are %s routines in the new version." % len(new_list)
f_341.close()



###------------ find the routines that are not in the old version
routines_new = []
for item in new_list:
    if item not in old_list:
        routines_new.append(item)
        
print "There are %s new routines." % len(routines_new)





###------------ print out the new routines
#i=0
#for item in routines_new:
#    i=i+1
#    print i, item
    




###------------ find 'Linear Solve' routines in the new version
routines_solve = []
for item in new_list:
    f = urllib.urlopen("http://www.netlib.org/lapack/lapack_routine/"+item)
    text = f.read()
    index = text.find("system of linear equations")
    if index > -1:
        routines_solve.append(item)
        #print item
    else:
        pass
    f.close()
    
print "There are %s routines with 'system of linear equations'." % len(routines_solve) 





###------------ find 'Linear Solve' routines that are NOT in the old version
routines_solve_new = []
for item in routines_solve:
    if item not in old_list:
        routines_solve_new.append(item)
        print item
print "There are %s new linear_solve routines." % len(routines_solve_new)





