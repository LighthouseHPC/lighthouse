import os, urllib, shutil, csv


###------------ make a list of the routines from the old version
def Old_List():
    old_list = []
    f_single = open("web/single_old.html")
    f_double = open("web/double_old.html")
    f_complex = open("web/complex_old.html")
    f_complex16 = open("web/complex16_old.html")

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






###------------ make a list of the routines from the new (3.4.1) version
def New_List():    
    new_list = []
    f_341 = open("web/routines_341.html")
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

