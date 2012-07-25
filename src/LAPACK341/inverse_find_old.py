import os, urllib, shutil, csv

###------------ make a list of routine url's for the old version
def Old_List_URL():
    old_list_url = []
    f_single = open("web/single_old.html")
    f_double = open("web/double_old.html")
    f_complex = open("web/complex_old.html")
    f_complex16 = open("web/complex16_old.html")

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


###------------find 'inverse' the old version
###------------ and write them into routines/routines_old_inverse.txt
## find the routines that HAVE the keywords:
f_inverse_old = open('routines/routines_old_inverse.txt', 'w')
routines_inverse_old = []
for url in old_list_url:
    f_info = urllib.urlopen(url)
    routineName = url.split("/")[-1]
    for line in f_info:
        index1 = line.find("inverse")
        if index1 > -1:
            routines_inverse_old.append(routineName)
            f_inverse_old.write(routineName+'\n')
        else:
            pass
    f_info.close()
    
routines_inverse_old = [x for x in routines_inverse_old if not routines_inverse_old.count(x) > 1]
print "There are %s routines in old_list that contain the keyword, 'inverse'." % len(routines_inverse_old)







