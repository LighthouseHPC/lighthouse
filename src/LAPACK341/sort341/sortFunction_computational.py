f_single = open('./routines/computational_341_single.txt', 'r')
f_double = open('./routines/computational_341_double.txt', 'r')
f_complex = open('./routines/computational_341_complex.txt', 'r')
f_complex16 = open('./routines/computational_341_complex16.txt', 'r')
f_aux = open('./routines/computational_341_aux.txt', 'r')

outputFile = open('./routines/functions_computational.txt', 'w')
sub = []
routineList = []
for line in f_single:
    routine_single = line.rstrip('\n').split('/')[5]
    routineList.append(routine_single)
    #finds the last four digists
    sub.append(routine_single[-6:])
    #print routine_single[-6:]
    
for line in f_double:
    routine_double = line.rstrip('\n').split('/')[5]
    routineList.append(routine_double)
    sub.append(routine_double[-6:])

    
for line in f_complex:
    routine_complex = line.rstrip('\n').split('/')[5]
    routineList.append(routine_complex)
    sub.append(routine_complex[-6:])
    
for line in f_complex16:
    routine_complex16 = line.rstrip('\n').split('/')[5]
    routineList.append(routine_complex16)
    sub.append(routine_complex16[-6:])
    
    
for line in f_aux:
    routine_aux = line.rstrip('\n').split('/')[5]
    routineList.append(routine_aux)
    sub.append(routine_aux[-6:])
    
    
    
# Remove duplicates with the 'set' module so that all the elements are unique and ordered.
# The ordered list is called sub.
sub = list(set(sub))


for element in sub:
    i=0
    for routine in routineList:
        if element in routine:
            i += 1
            outputFile.write(routine)
    # 'i' indicates the total number of the routines that end with the same'element'.         
    outputFile.write("------------------------------------------------------ %s \n \n" % i)
    

print "len(sub) = ",len(sub)
print "Total number of Computational routines in v3.4.1: ", len(routineList)

