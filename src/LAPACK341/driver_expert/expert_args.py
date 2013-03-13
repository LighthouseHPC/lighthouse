import os, glob, csv


""" open routines/expert_341.txt and make a list of the file names. """
expertList_341 = []
f = open('./routines/expert_341.txt')
for line in f:
    expertList_341.append(line.strip('\r\n'))
    
print expertList_341, len(expertList_341)


resultFile = open("parameters_expert.csv",'wb')
wr = csv.writer(resultFile, dialect='excel')
for infile in expertList_341:
    parameters = {'in': [], 'out': [], 'inout': []}
    results = []
    ftxt = open('../../../Dlighthouse/media/Doxygen/lapack/'+infile)
    for line in ftxt:
        if '\param[in]' in line:
                parameters['in'].append(line.split()[-1])
        if '\param[out]' in line:
                parameters['out'].append(line.split()[-1])
        if '\param[in,out]' in line:
                parameters['inout'].append(line.split()[-1])
    #print parameters
    results = [[infile, parameters['in'], parameters['out'], parameters['inout']]]
    wr.writerows(results)
    
f.close()
ftxt.close()
resultFile.close()