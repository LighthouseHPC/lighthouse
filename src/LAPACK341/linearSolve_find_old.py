import os, urllib, shutil, csv

###------------ make a list of routine url's for the old version
def Old_List_URL():
    old_list_url = []
    f_single = open("single_old.html")
    f_double = open("double_old.html")
    f_complex = open("complex_old.html")
    f_complex16 = open("complex16_old.html")

    for line in f_single:
        if "href=" in line:
            old_list_url.append("http://www.netlib.org/lapack/single/"+line.split('\"')[1])
        else:
            pass
       
    for line in f_double:
        if "href=" in line:
            old_list_url.append("http://www.netlib.org/lapack/double/"+line.split('\"')[1])
        else:
            pass
    
    for line in f_complex:
        if "href=" in line:
            old_list_url.append("http://www.netlib.org/lapack/complex/"+line.split('\"')[1])
        else:
            pass
    
    for line in f_complex16:
        if "href=" in line:
            old_list_url.append("http://www.netlib.org/lapack/complex16/"+line.split('\"')[1])
        else:
            pass
    
    f_single.close()
    f_double.close()
    f_complex.close()
    f_complex16.close()
    return old_list_url




old_list_url = Old_List_URL()


###------------find 'Linear Solve' and "solves the equation" routines in the old version
###------------ and write them into routines/routines_old_linearSolve.txt
## find the routines that HAVE the keywords:
f_linearSolve_old = open('routines/routines_old_linearSolve.txt', 'w')
routines_solve_old = []
for url in old_list_url:
    f_info = urllib.urlopen(url)
    routineName = url.split("/")[-1]
    if 'rfs.f' in routineName:
        pass
    else:
        for line in f_info:
            index1 = line.find("system of linear")
            if index1 > -1:
                routines_solve_old.append(routineName)
                f_linearSolve_old.write(routineName+'\n')
            else:
                index2 = line.find("solves the equation")
                if index2 > -1:
                    routines_solve_old.append(routineName)
                    f_linearSolve_old.write(routineName+'\n')
                else:
                    index3 = line.find("system of the form")
                    if index3 > -1:
                        routines_solve_old.append(routineName)
                        f_linearSolve_old.write(routineName+'\n')
                    else:
                        index4 = line.find("systems of equations")
                        if index4 > -1:
                            routines_solve_old.append(routineName)
                            f_linearSolve_old.write(routineName+'\n')
                        else:
                            pass
    f_info.close()
    
routines_solve_old = [x for x in routines_solve_old if not routines_solve_old.count(x) > 1]
print "There are %s routines in old_list that contain the keywords for le_solve." % len(routines_solve_old)







