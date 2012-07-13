import os, urllib, shutil, csv

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




###------------ find the routines that are in both versions
#print set(old_list) & set(new_list)
print "There are %s overlapped routines." % len(set(old_list) & set(new_list))




###------------ find the routines that are not in the old version
routines_new = list(set(new_list) - set(old_list))
print "There are %s new routines." % len(routines_new)




'''
###------------ find 'Linear Solve' routines in the new version and
###------------ write them into routines/routines_341_linearSolve.txt
wr = csv.writer(open('routines/routines_341_linearSolve.txt', 'w'), delimiter=';')

routines_solve = []
i=0
for item in new_list:
    f = urllib.urlopen("http://www.netlib.org/lapack/lapack_routine/"+item)
    text = f.read()
    index1 = text.find("system of linear equations")
    if index1 > -1:
        i = i+1
        routines_solve.append(item)
        wr.writerow([i, item[0], item[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+item])
    else:
        index2 = text.find("solves the equation")
        if index2 > -1:
            i = i+1
            routines_solve.append(item)
            wr.writerow([i, item[0], item[1:-2], "http://www.netlib.org/lapack/lapack_routine/"+item])
        else:
            pass
    f.close()

print "There are %s routines with 'system of linear equations'." % len(routines_solve) 

'''



###------------ find 'Linear Solve' routines that are NOT in the old version and
###------------ write them into routines/routines_341_linearSolve_new.txt
f_linearSolve = open('routines/routines_341_linearSolve.txt')
f_linearSolve_new = open('routines/routines_341_linearSolve_new.txt', 'w')
for line in f_linearSolve:
    item = line.split(";")[-1].split("/")[-1].split(".f")[0]+'.f'
    if item not in old_list:
        print item
        f_linearSolve_new.write(item+'\n')
    else:
        pass

f_linearSolve.close()
f_linearSolve_new.close()



