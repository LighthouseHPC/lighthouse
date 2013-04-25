import os, fnmatch, re

from lighthouse.models.lapack_le import lapack_le_driver



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
        "UPLO": "WRITE(*, '(A)', ADVANCE = 'NO') 'Input U for upper triangular matrix or L for lower triangular matrix. UPLO = '\n\t    READ *, UPLO \n",
        "N": "WRITE(*, '(A)', ADVANCE = 'NO') 'Input the order of your square matrix. N = '\n\t    READ *, N \n",
        "NRHS": "WRITE(*, '(A)', ADVANCE = 'NO') 'Input number of columns in B. NRHS = '\n\t    READ *, NRHS \n",
        "LDA": "WRITE(*, '(A)', ADVANCE = 'NO') 'Input leading dimension of A, LDA = '\n\t    READ *, LDA \n",
        "LDB": "WRITE(*, '(A)', ADVANCE = 'NO') 'Input leading dimension of B, LDB = '\n\t    READ *, LDB \n",
        "KD": "WRITE(*, '(A)', ADVANCE = 'NO') 'Input the number of superdiagonals(or subdiagonals) of the matrix A if UPLO = 'U'(or UPLO = 'L'). KD = '\n\t    READ *, KD \n",
        "KL": "WRITE(*, '(A)', ADVANCE = 'NO') 'Input the number of subdiagonals within the band of the matrix. KL = '\n\t    READ *, KL \n",
        "KU": "WRITE(*, '(A)', ADVANCE = 'NO') 'Input the number of superdiagonals within the band of the matrix. KU = '\n\t    READ *, KU \n", 
         }



class generateTemplate(object):
    __name__ = 'generateTemplate'
    
    def __init__(self, routineName):
        self.routineName = routineName
        
    def sort_parameters(self):
        listing = os.listdir(txtpath)
        parameters = {'call': '', 'real_1D': [], 'dataType': [], 'integer': [], 'array_1D_int': [],
                        'array_1D': [], 'matrix': [], 'character': []}
        for file in listing:
            if fnmatch.fnmatch(file, self.routineName+'.f'):
                file_abs = os.path.join(txtpath, file)
                with open(file_abs,'r') as opentxt:
                    for line in opentxt:
                        line = line.strip('*').strip()
                        if 'SUBROUTINE' in line:
                            line = line + opentxt.next().strip('*').strip()
                            parameters['call'] = line.replace("SUBROUTINE", "CALL")
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
                                    parameters['character'].append(item)
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
        ### --- copy sample file to test1.f90 --- ###
        with open("./lighthouse/templateGen/fortran/codeTemplates/test1.f90", "w") as f:
            with open("./lighthouse/templateGen/fortran/baseCode/driver_simple_head.txt", "r") as f_head:
                for line in f_head.readlines():
                    f.write(line)
                    
                
        ### --- set up input question subprogram --- ###
        ALLOCATE = []
        with open("./lighthouse/templateGen/fortran/codeTemplates/test1.f90", "a") as f:
            f.write('\n\n')
            f.write('\tSUBROUTINE DIMNS_ASSIGNMENT\n')
            f.write('\t    USE Declaration\n')
            f.write('\t    !--- obtain inputs ---!\n')
            for key, value in inputQ.iteritems():
                if key in self.sort_parameters()['character']+self.sort_parameters()['integer']:
                    f.write('\t    '+value)
            f.write('\n')
            
            if 'LWORK' in self.sort_parameters()['integer']:
                f.write('\t    LWORK = N*N\n')
                f.write('\n')
                
            if 'LDAB' in self.sort_parameters()['integer']:
                f.write('\t    ldab = 2*KL+KU+1\n')
                f.write('\n')
            
        ### --- set up ALLOCATE and prepare for deallocate --- ###
            f.write('\t    !--- allocate matrix/array ---!\n')
            for key, value in allocate.iteritems():
                if key in self.sort_parameters()['array_1D_int']+self.sort_parameters()['array_1D']+self.sort_parameters()['matrix']:
                    f.write("\t    ALLOCATE(%s) \n"%value)
                    ALLOCATE.append(key)
            f.write('\tEND SUBROUTINE DIMNS_ASSIGNMENT\n')
            f.write('\n\n')
        ## --- read data --- ###
            f.write('\tSUBROUTINE GET_DATA\n')
            f.write('\t    USE Declaration\n')
            for item in ALLOCATE:
                flag = 1
                with open("./lighthouse/templateGen/fortran/baseCode/readAB.txt", "r") as f_readAB:
                    if item == 'AB' and 'gbsv' in self.routineName:
                        for line in f_readAB.readlines():
                            if "begin AB(gbsv)" in line:
                                flag = 0
                            if "end AB(gbsv)" in line:
                                flag = 1
                            if not flag and not "begin AB(gbsv)" in line:
                               f.write(line)
                    elif item == 'IPIV':
                        pass
                    else:
                        for line in f_readAB.readlines():
                            if "begin %s\n"%item in line:
                                flag = 0
                            if "end %s\n"%item in line:
                                flag = 1
                            if not flag and not "begin %s\n"%item in line:
                               f.write(line)
            f.write('\tEND SUBROUTINE GET_DATA\n')
                    
            
        ### --- Combine with driver_simple_tail.txt --- ###
        with open("./lighthouse/templateGen/fortran/codeTemplates/test1.f90", "a") as f:
            with open("./lighthouse/templateGen/fortran/baseCode/driver_simple_tail.txt", "r") as f_tail:
                for line in f_tail.readlines():
                    f.write(line)
    
                    
        ### --- final fixes --- ###
        """ create a dictionary for replacing strings in the original file. """
        replaceDict = {'routineName': self.routineName,
                       'dataType': self.sort_parameters()['dataType'][0],
                       'KIND=': self.sort_parameters()['dataType'][1],
                       'integer_list': ', '.join(self.sort_parameters()['integer']),
                       'matrix_list': ', '.join(self.sort_parameters()['matrix']),
                       'ALLOCATE_list': ','.join(ALLOCATE),
                       'routine_function': self.sort_parameters()['call']
                       }
        
        with open("./lighthouse/templateGen/fortran/codeTemplates/test2.f90", "wt") as fout:
            with open("./lighthouse/templateGen/fortran/codeTemplates/test1.f90", "r") as fini:
                fout.write(replacemany(replaceDict, fini.read()))
                
        f_read = open("./lighthouse/templateGen/fortran/codeTemplates/test2.f90", "r")
        lines = f_read.readlines()
        f_read.close()
        f_write = open("./lighthouse/templateGen/fortran/codeTemplates/temp_%s.f90"%self.routineName,"w")
        for line in lines:
            if 'array_1D_int_list' in line:
                if self.sort_parameters()['array_1D_int']:
                    line = line.replace('array_1D_int_list', ', '.join(self.sort_parameters()['array_1D_int']))
                    f_write.write(line)
                else:
                    pass
            elif 'character_list' in line:
                if self.sort_parameters()['character']:
                    line = line.replace('character_list', ', '.join(self.sort_parameters()['character']))
                    f_write.write(line)
                else:
                    pass   
            elif 'real_1D_list' in line:
                if self.sort_parameters()['real_1D']:
                    line = line.replace('real_1D_list', ', '.join(self.sort_parameters()['real_1D']))
                    f_write.write(line)
                else:
                    pass 
            elif 'array_1D_list' in line:
                if self.sort_parameters()['array_1D']:
                    line = line.replace('array_1D_list', ', '.join(self.sort_parameters()['array_1D']))
                    f_write.write(line)
                else:
                    pass
            elif 'input_lines' in line:
                q_list = []
                for key, value in inputQ.iteritems():
                    if key in self.sort_parameters()['character']+self.sort_parameters()['integer']:
                        q_list.append(value)
                print q_list
                line = line.replace('input_lines', ''.join(q_list))
            elif 'fmtNum' in line:
                if self.routineName.startswith('s') or self.routineName.startswith('d'):
                    line = line.replace('fmtNum', '11100')
                else:
                    line = line.replace('fmtNum', '22200')
                f_write.write(line)
            else:
                f_write.write(line)
                    
        """ remove test files """
        os.remove('./lighthouse/templateGen/fortran/codeTemplates/test1.f90')
        os.remove('./lighthouse/templateGen/fortran/codeTemplates/test2.f90')
        
        



def replacemany(adict, astring):
    pat = '|'.join(re.escape(s) for s in adict)
    there = re.compile(pat)
    def onerepl(mo):
        return adict[mo.group()]
    return there.sub(onerepl, astring)
