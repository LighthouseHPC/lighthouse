f = open('./routines/computational_341_single.txt', 'r')
outputFile = open('functions.txt', 'w')
sub = []
for line in f:
    #finds the last four digists
    sub.append( (line[len(line)-7 : len(line) - 3]) )
    
    
# Remove duplicates with the 'set' module so that all the elements are unique and ordered.
# The ordered list is called sub1.
sub1 = list(set(sub))


# I suppose that you try to remove duplicates here. Let's call it PROCESS_COUNT.
i = 0
while (i < len(sub)):
    count = sub.count(sub[i])
    if ( count > 1 ):
        while (count > 1):
            sub.remove(sub[i])
            count = count - 1
            #print the result for checking...
            #print sub[i], count
    else:
       i = i + 1

sub.sort()


for element in sub:
    f.seek(0,0)
    i = 0
    for line in f:
        if (line[len(line)-7 : len(line) - 3] == element):
            i += 1
            routine = line.split('/')[5]
            outputFile.write(routine)
    # 'i' indicates the total number of the routines that end with the same'element'.         
    outputFile.write("------------------------------------------------------ %s \n \n" % i)
    


#If you check functions.txt, you will see some routines in the original 'sub' list are missing after PROCESS_COUNT.
#A simple check:
print "len(sub) = ",len(sub)
print "len(sub1) = ", len(sub1)

#find the elements that are in sub1 but not in sub(after PROCESS_COUNT):
print "The missing elements are:"
print list(set(sub1).symmetric_difference(set(sub)))