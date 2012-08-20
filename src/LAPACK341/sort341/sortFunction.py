f = open('./routines/computational_341_single.txt', 'r')
outputFile = open('routines/functions.txt', 'w')
sub = []
for line in f:
    #finds the last four digists
    sub.append( (line[len(line)-7 : len(line) - 3]) )

#i = 0
#while (i < len(sub)):
#    count = sub.count(sub[i])
#    if ( count > 1 ):
#        while (count > 1):
#            sub.remove(sub[i])
#            count = count - 1
#    else:
#       i = i + 1

sub = list(set(sub))

#sub.sort()

for element in sub:
    f.seek(0,0)
    i = 0
    for line in f:
        if (line[len(line)-7 : len(line) - 3] == element):
            i += 1
            routine = line.split('/')[5]
            outputFile.write(routine)
    outputFile.write("------------------------------------------------------ %s \n \n" % i)
    

