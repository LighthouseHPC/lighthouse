import os, fnmatch, re

from lighthouse.models.lapack_le import lapack_le_arg

fortran_path = './lighthouse/templateGen/fortran/'

keyword_list = ['sv', 'trf', 'trs', 'con', 'tri', 'rfs',]

class generateTemplate(object):
    __name__ = 'generateTemplate'
    
    def __init__(self, routineName):
        self.routineName = routineName
        
        
    def get_dataType(self):
        dataType = []
        if self.routineName.startswith('s'):
            dataType.extend(['REAL', 'KIND=4'])
        elif self.routineName.startswith('d'):
            dataType.extend(['REAL', 'KIND=8'])
        elif self.routineName.startswith('c'):
            dataType.extend(['COMPLEX', 'KIND=4'])
        elif self.routineName.startswith('z'):
            dataType.extend(['COMPLEX', 'KIND=8'])
        return dataType

            
    def get_database(self):
        ROUTINE = lapack_le_arg.objects.filter(routineName__icontains=self.routineName)
        ### --- determin problem type: sv, trf, trs, con, tri, rfs ---###
        for item in keyword_list:
            if item in self.routineName:
                keyword = item
        
        ### --- set up important parameters --- ###
        if keyword in ['sv', 'trf']:
            routineName_trf = ''
            trf_parameters = ''
            routineName_trs = ''
            trs_parameters = ''
            question_list = ROUTINE[0].param_in.split(',')
        else:
            routineName_trf = self.routineName.replace(keyword, 'trf')
            trf_parameters = lapack_le_arg.objects.filter(routineName__icontains=routineName_trf)[0].param_all
            if keyword == 'con':
                question_list = list(set(lapack_le_arg.objects.filter(routineName__icontains=routineName_trf)[0].param_in.split(','))|set(ROUTINE[0].param_in.split(','))|set(ROUTINE[0].char.split(',')))
                routineName_trs = ''
                trs_parameters = ''
            elif keyword == 'rfs':
                question_list = list(set(lapack_le_arg.objects.filter(routineName__icontains=routineName_trf)[0].param_in.split(','))|set(ROUTINE[0].param_in.split(',')))
                trf_parameters = trf_parameters.replace('A', 'AF')
                routineName_trs = self.routineName.replace(keyword, 'trs')
                trs_parameters = lapack_le_arg.objects.filter(routineName__icontains=routineName_trs)[0].param_all.replace('A', 'AF').replace('B', 'X')
            else:
                question_list = list(set(lapack_le_arg.objects.filter(routineName__icontains=routineName_trf)[0].param_in.split(','))|set(ROUTINE[0].param_in.split(',')))
                routineName_trs = ''
                trs_parameters = ''
            question_list = sorted(question_list, reverse=True)
                
        databaseInfo = {'keyword': keyword, 'routine': ROUTINE, 'questionList': question_list,
                        'routineTrf': routineName_trf, 'trfParameters': trf_parameters,
                        'routineTrs': routineName_trs, 'trsParameters': trs_parameters}
        return databaseInfo
    
    
    def make_template(self):
        ### --- get data from database --- ### 
        ROUTINE = self.get_database()['routine']
        keyword = self.get_database()['keyword']
        question_list = self.get_database()['questionList']
        routineName_trf = self.get_database()['routineTrf']
        trf_parameters = self.get_database()['trfParameters']
        routineName_trs = self.get_database()['routineTrs']
        trs_parameters = self.get_database()['trsParameters']
        #print question_list

        
        ### --- copy head.txt file to test1.f90 --- ###
        with open(fortran_path+"codeTemplates/test1_"+self.routineName+".f90", "w") as f:
            with open(fortran_path+"baseCode/head_"+keyword+".txt", "r") as f_head:
                for line in f_head.readlines():
                    f.write(line)
        
        ### --- create SUBROUTINE DIMNS_ASSIGNMENT --- ###           
            ## --- set up input question subprogram by reading from readQ.txt --- ##
            f.write('\n\n')
            f.write('\tSUBROUTINE DIMNS_ASSIGNMENT\n')
            f.write('\t    USE Declaration\n')
            f.write('\t    !--- obtain inputs ---!\n')
            for item in question_list:
                flag = 1
                with open(fortran_path+"baseCode/readQ.txt", "r") as f_readQ:
                    for line in f_readQ.readlines():
                        if "begin %s\n"%item in line:
                            flag = 0
                        if "end %s\n"%item in line:
                            flag = 1
                        if not flag and not "begin %s\n"%item in line:
                           f.write(line)
            
            if ROUTINE[0].LDA_condition:
                f.write('\n')
                for item in ROUTINE[0].LDA_condition.split(';'):
                    f.write('\t    %s\n'%item)
            
            ## --- set up ALLOCATE --- ##
            allocate_list = ROUTINE[0].allocate.split(";")
            f.write('\n')
            f.write('\t    !--- allocate matrix/array ---!\n')
            for item in allocate_list:
                f.write('\t    ALLOCATE(%s)\n'%item)
            f.write('\tEND SUBROUTINE DIMNS_ASSIGNMENT\n')
            
            
        ### --- create SUBROUTINE GET_DATA --- ###
            readData_list = ROUTINE[0].readData.split(';')
            f.write('\n\n')
            f.write('\tSUBROUTINE GET_DATA\n')
            f.write('\t    USE Declaration\n')
            f.write('\t    !--- read data from files ---!\n')
            if ROUTINE[0].readData_L:
                f.write("\t    IF (UPLO == 'U') THEN\n")
                f.write("\t        READ(11, *) %s\n"%readData_list[0])
                f.write("\t    ELSE IF (UPLO == 'L') THEN\n")
                f.write("\t        READ(11, *) %s\n"%ROUTINE[0].readData_L.strip('\"'))
                f.write("\t    END IF\n\n")
            else:
                for item in readData_list:
                    f.write('\t    READ(11, *) %s\n\n'%item)

            if keyword in ['sv', 'trs', 'rfs']:
                f.write('\t    !--- read data from file for B ---!\n')
                f.write('\t    READ(22, *) ((B(I,J),J=1,NRHS),I=1,LDB)\n')
                
            f.write('\tEND SUBROUTINE GET_DATA\n\n\n')
            
            
        ### --- Combine with tail.txt file--- ###
        if not keyword == 'con':
            with open(fortran_path+"codeTemplates/test1_"+self.routineName+".f90", "a") as f:
                if keyword in ['sv', 'trf', 'tri']:
                    with open(fortran_path+"baseCode/tail_"+keyword+".txt", "r") as f_tail:
                        flag = 1
                        for line in f_tail.readlines():
                            if "begin" in line and self.routineName[1:] in line.split():
                                flag = 0
                            if "end" in line and self.routineName[1:] in line.split():
                                flag = 1
                            if not flag and not "begin" in line and not self.routineName[1:] in line.split():
                               f.write(line)                  
                else:
                    with open(fortran_path+"baseCode/tail_"+keyword+".txt", "r") as f_tail:
                        for line in f_tail.readlines():
                            f.write(line)
        
            
            
        ### --- final fixes --- ###
        ## --- set up format number for printing matrix --- ##
        if self.routineName.startswith('s') or self.routineName.startswith('d'):
            fmtNum = '11100'
        else:
            fmtNum = '22200'
            
        ## --- create a dictionary for replacing strings in the original file. --- ##
        replaceDict = {'routineName': self.routineName,
                       'routine_parameters': ROUTINE[0].param_all,
                       'dataType': self.get_dataType()[0],
                       'KIND=': self.get_dataType()[1],
                       'integer_list': ROUTINE[0].integers,
                       'real_list': ROUTINE[0].reals,
                       'array_1D_int_list': ROUTINE[0].array_1d_int,
                       'character_list': ROUTINE[0].char,
                       'real_1D_list': ROUTINE[0].array_1d_real,
                       'matrix_list': ROUTINE[0].matrix,
                       'array_1D_list': ROUTINE[0].array_1d,
                       'ALLOCATE_list': ROUTINE[0].allocate_list,
                       'fmtNum': fmtNum,
                       'routineName_trf': routineName_trf,
                       'trf_parameters': trf_parameters,
                       'routine_anorm': self.routineName[0]+ROUTINE[0].other[0:5],
                       'anorm_param': self.routineName[0]+ROUTINE[0].other,
                       'routineName_trs': routineName_trs,
                       'trs_parameters': trs_parameters,
                       }

        with open(fortran_path+"codeTemplates/test2_"+self.routineName+".f90", "wt") as fout:
            with open(fortran_path+"codeTemplates/test1_"+self.routineName+".f90", "r") as fini:
                fout.write(replacemany(replaceDict, fini.read()))


        ### --- delete lines containing empty allocate lists and copy test2.f90 to the final temp_xxxxx.f90 file. --- ### 
        f_read = open(fortran_path+"codeTemplates/test2_"+self.routineName+".f90", "r")
        lines = f_read.readlines()
        f_read.close()
        f_write = open(fortran_path+"codeTemplates/temp_%s.f90"%self.routineName,"w")
        for line in lines:
            if ':: \n' not in line:
                f_write.write(line)
        
                
        ### --- remove test files --- ###
        os.remove(fortran_path+"codeTemplates/test1_"+self.routineName+".f90")
        os.remove(fortran_path+"codeTemplates/test2_"+self.routineName+".f90")





def replacemany(adict, astring):
    pat = '|'.join(re.escape(s) for s in adict)
    there = re.compile(pat)
    def onerepl(mo):
        return adict[mo.group()]
    return there.sub(onerepl, astring)
