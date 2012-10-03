from Base import BaseServer,BaseClient
from utils import change_workdir, remove_workdir
import os,os.path, glob

########################################################################
class BTO_Server(BaseServer):
    
    def __init__(self, u=[],r='1', btodir='../../bto', btoblas='./bin/btoblas'):
        self.legal_options = '-e'
        self.legal_longoptions = ''
        self.users = u
        self.req_id = r
        self.bto_dir = btodir
        self.bto_blas = btoblas


class BTORequestHandler(BaseServer):
    
    def __init__(self, legal_options, legal_longoptions, u,r, btodir, btoblas):
        self.legal_options = legal_options
        self.legal_longoptions = legal_longoptions
        self.users = u
        self.req_id = r
        self.bto_dir = btodir
        self.bto_blas = btoblas
        
    def bto_handle(self):
        userid  = self.check_user(self.recv_header1())
        options = self.check_options(self.recv_header1())
        nfiles  = int(self.recv_header1())
        baseworkdir = userid+"_"+self.req_id
        try:
            os.chdir('temp')
            change_workdir(baseworkdir)
            workdir = os.getcwd()
        except:
            print('Error creating temporary directory')
        files = self.recv_files(nfiles)
        filename = files[0]
        
        os.chdir(self.bto_dir)
        os.system(self.bto_blas + ' ' + options + ' ' + workdir + '/' +filename)
        os.chdir(workdir)
        cfiles = glob.glob('*.c')
        if(len(cfiles) == 1): 
            self.send_header1(1)
            self.send_files(cfiles)
        else:
            self.send_error('error generating c file')
        remove_workdir(baseworkdir)

    
class BTO_Client(BaseClient):
        
    def submit_request(self, host, port, user, options,file1):
        files = [file1]
        BaseClient.submit_request(self, host, port, user, options, files)       
