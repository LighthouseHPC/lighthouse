import os, glob, csv


""" open routines/solve_341.txt and make a list of the file names. """
solveList_341 = []
f = open('./routines/solve_341.txt')
for line in f:
    solveList_341.append(line.strip('\r\n'))
    
print solveList_341, len(solveList_341)


resultFile = open("parameters_solve.csv",'wb')
wr = csv.writer(resultFile, dialect='excel')
for infile in solveList_341:
    if not infile in ['sptts2.f', 'dptts2.f', 'cptts2.f', 'zptts2.f']:
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