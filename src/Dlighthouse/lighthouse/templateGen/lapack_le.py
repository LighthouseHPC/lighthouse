import os, fnmatch, re

txtpath = 'media/Doxygen/lapack'

integer = ['N', 'NRHS', 'LDA', 'LDB', 'LDAB', 'INFO', 'KD', 'KL', 'KU', 'LWORK']
array_1D_int = ['IPIV', 'WORK']
array_1D = ['DL', 'D', 'DU', 'AP', 'E']
matrix = ['A', 'B', 'AB']
character = ['UPLO']
real_1D = ['D']
allocate = {'IPIV': 'IPIV(N)', 'WORK': 'WORK(LWORK)',
            'DL': 'DL(N-1)', 'D': 'D(N)', 'DU': 'DU(N-1)', 'AP': 'AP(N*(N+1)/2)', 'E': 'E(N-1)',
            'A':'A(LDA,N)', 'B': 'B(LDB,NRHS)', 'AB':'AB(LDAB,N)',}

inputQ = {
        "UPLO": "PRINT *, 'Input 'U' for upper triangular matrix or 'L' for lower triangular matrix. UPLO = '\nREAD *, UPLO \n",
        "N": "PRINT *, 'Input the order of your square matrix. N = '\nREAD *, N \n",
        "NRHS": "PRINT *, 'Input number of columns in B. NRHS = '\nREAD *, NRHS \n",
        "LDA": "PRINT *, 'Input leading dimension of A, LDA = '\nREAD *, LDA \n",
        "LDB": "PRINT *, 'Input leading dimension of B, LDB = '\nREAD *, LDB \n",
        "LDAB": "PRINT *, 'Input leading dimension of AB, LDAB = '\nREAD *, LDAB \n",
        "KD": "PRINT *, 'Input the number of superdiagonals(or subdiagonals) of the matrix A if UPLO = 'U'(or UPLO = 'L'). KD = '\nREAD *, KD \n",
         
         }


    
def replacemany(adict, astring):
    pat = '|'.join(re.escape(s) for s in adict)
    there = re.compile(pat)
    def onerepl(mo):
        return adict[mo.group()]
    return there.sub(onerepl, astring)


class generateTemplate(object):
    __name__ = 'generateTemplate'
    
    def __init__(self, routineName):
        self.routineName = routineName
        
    def sort_parameters(self):
        listing = os.listdir(txtpath)
        parameters = {'call': '', 'real_1D': [], 'dataType': [], 'integer': [], 'array_1D_int': [], 'array_1D': [], 'matrix': [], 'characher': []}
        for file in listing:
            if fnmatch.fnmatch(file, self.routineName+'.f'):
                file_abs = os.path.join(txtpath, file)
                with open(file_abs,'r') as opentxt:
                    for line in opentxt:
                        if 'SUBROUTINE' in line:
                            parameters['call'] = line.replace("*       SUBROUTINE", "CALL")
                            for item in line.split('(')[1].strip(')').strip('\n)').replace(' ', '').split(','):
                                if item in integer:
                                    parameters['integer'].append(item)
                                elif item in array_1D_int:
                                    parameters['array_1D_int'].append(item)
                                elif item in array_1D:
                                    parameters['array_1D'].append(item)
                                elif item in matrix:
                                    parameters['matrix'].append(item)
                                elif item in character:
                                    parameters['characher'].append(item)
                                elif item in real_1D:
                                    parameters['real_1D'].append(item)
                        elif '\par Purpose' in line:
                            break
                        else:
                            pass
        if self.routineName.startswith('s'):
            parameters['dataType'].extend(['REAL', 'KIND=4'])
        elif self.routineName.startswith('d'):
            parameters['dataType'].extend(['REAL', 'KIND=8'])
        elif self.routineName.startswith('c'):
            parameters['dataType'].extend(['COMPLEX', 'KIND=4'])
        elif self.routineName.startswith('z'):
            parameters['dataType'].extend(['COMPLEX', 'KIND=8'])
        
        """ for xPTSV only """
        if 'ptsv' in self.routineName:
            parameters['real_1D'] = ['D']
            parameters['array_1D'].remove('D')
        
        
        return parameters

            
    def make_template(self):
        #print self.sort_parameters()
        """ create a dictionary for replacing strings in the original file. """
        replaceDict = {'routineName': self.routineName,
                       'dataType': self.sort_parameters()['dataType'][0],
                       'KIND=': self.sort_parameters()['dataType'][1],
                       'integer_list': ', '.join(self.sort_parameters()['integer']),
                       'matrix_list': ', '.join(self.sort_parameters()['matrix']),
                       'CALL': self.sort_parameters()['call'],
                       }
        
        #replaceDict2 = {'array_1D_int_list': ', '.join(self.sort_parameters()['array_1D_int']),
        #               'real_1D_list': self.sort_parameters()['real_1D'][0],
        #               'array_1D_list': ', '.join(self.sort_parameters()['array_1D_int_list'])
        #               }
       
       
        """ set up parameters """
        with open("./lighthouse/templateGen/temp_fortran/test.txt", "wt") as fout:
            with open("./lighthouse/templateGen/temp_fortran/driver_simple.txt", "r") as fini:
                fout.write(replacemany(replaceDict, fini.read()))
                
        """ set up input questions """
        print self.sort_parameters()['characher']+self.sort_parameters()['integer']
        with open("./lighthouse/templateGen/temp_fortran/test_q.txt", "w") as f:
            for key, value in inputQ.iteritems():
                if key in self.sort_parameters()['characher']+self.sort_parameters()['integer']:
                    f.write(value)
                    
                    
        """ set up ALLOCATE """
        deALLOCATE = []
        with open("./lighthouse/templateGen/temp_fortran/test_a.txt", "w") as f:
            for key, value in allocate.iteritems():
                if key in self.sort_parameters()['array_1D_int']+self.sort_parameters()['array_1D']+self.sort_parameters()['matrix']:
                    f.write("ALLOCATE(%s) \n"%value)
                    deALLOCATE.append(key)
        print deALLOCATE
                    
        return self.sort_parameters()
    


