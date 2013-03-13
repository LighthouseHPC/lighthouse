import os, glob, csv


""" open routines/condNumber_341.txt and make a list of the file names. """
condNumberList_341 = []
f = open('./routines/condNumber_341.txt')
for line in f:
    condNumberList_341.append(line.strip('\r\n'))
    
print condNumberList_341, len(condNumberList_341)


resultFile = open("parameters_condNumber.csv",'wb')
wr = csv.writer(resultFile, dialect='excel')
for infile in condNumberList_341:
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