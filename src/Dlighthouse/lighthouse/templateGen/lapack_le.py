import os, fnmatch, re

txtpath = 'media/Doxygen/lapack'

integer = ['N', 'NRHS', 'LDA', 'LDB', 'LDAB', 'INFO', 'KD', 'KL', 'KU', 'LWORK']
array_int = ['IPIV', 'WORK']
array_1D = ['DL', 'D', 'DU', 'AP', 'E']
matrix = ['A', 'B', 'AB']
character = ['UPLO']
real_1D = ['D']

input_sv = ["print *, 'Input the order of your square matrix. n = '", "read *, n", "print *, 'Input number of columns in b. nrhs = '", "read *, nrhs"]

class generateTemplate(object):
    __name__ = 'generateTemplate'
    
    def __init__(self, routineName):
        self.routineName = routineName
        
    def sort_parameters(self):
        listing = os.listdir(txtpath)
        parameters = {'call': '', 'real_1D': [], 'dataType': [], 'integer': [], 'array_int': [], 'array_1D': [], 'matrix': [], 'characher': []}
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
                                elif item in array_int:
                                    parameters['array_int'].append(item)
                                elif item in array_1D:
                                    parameters['array_1D'].append(item)
                                elif item in matrix:
                                    parameters['matrix'].append(item)
                                elif item in characher:
                                    parameters['characher'].append(item)
                                elif item in real_1D:
                                    parameters['real_1D'].append(item)
                        elif '\par Purpose' in line:
                            break
                        else:
                            pass
        if self.routineName.startswith('s'):
            parameters['dataType'].extend(['REAL', 4])
        elif self.routineName.startswith('d'):
            parameters['dataType'].extend(['REAL', 8])
        elif self.routineName.startswith('c'):
            parameters['dataType'].extend(['COMPLEX', 4])
        elif self.routineName.startswith('z'):
            parameters['dataType'].extend(['COMPLEX', 8])
        
        """ for xPTSV only """
        if 'ptsv' in self.routineName:
            parameters['real_1D'] = ['D']
            parameters['array_1D'].remove('D')
        
        
        return parameters

            
    def make_template(self):
        with open("./lighthouse/templateGen/temp_fortran/test.txt", "wt") as out:
            for line in open("./lighthouse/templateGen/temp_fortran/driver_simple.txt"):
                if 'routineName' in line:
                    out.write(line.replace('routineName', '%s')%self.routineName)
                elif 'integer_list' in line:
                    out.write(line.replace('integer_list', '%s')%', '.join(self.sort_parameters()['integer']))
                elif 'matrix_list' in line:
                    line = line.replace('dataType', '%s')%self.sort_parameters()['dataType'][0]
                    line = line.replace('matrix_list', '%s')%', '.join(self.sort_parameters()['matrix'])
                    line = line.replace('kindNum', '%s')%self.sort_parameters()['dataType'][1]
                    out.write(line)
                elif 'kindNum' in line:
                    line = line.replace('kindNum', '%s')%self.sort_parameters()['dataType'][1]
                    out.write(line)
                elif 'real_1D_list' in line and not self.sort_parameters()['real_1D']:
                    pass
                elif 'UPLO' in line and not self.sort_parameters()['characher']:
                    pass
                else:
                    out.write(line)
                    
        return self.sort_parameters()