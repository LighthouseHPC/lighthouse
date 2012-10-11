from Base import BaseServer,BaseClient
from utils import change_workdir, remove_workdir
from subprocess import call
import os,os.path, glob

########################################################################
class BTO_Server(BaseServer):
    
    def __init__(self, btodir, u ,r , btoblas='./bin/btoblas'):
        self.legal_options = '-e'
        self.legal_longoptions = ''
        self.bto_dir = btodir
        self.users = u
        self.req_id = r
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
        os.chdir('/tmp')        
        try:
            userid  = self.check_user(self.recv_header1())
            options = self.check_options(self.recv_header1())
            nfiles  = int(self.recv_header1())
            baseworkdir = userid+"_"+self.req_id
            os.mkdir(baseworkdir)
            os.chdir(baseworkdir)
            workdir = os.getcwd()
            files = self.recv_files(nfiles)
            filename = files[0]
        except:
            with open('errors.x', 'w') as f:
                f.write('An error occurred while the BTO server was receiving the input file.')
            self.send_header1(1)
            self.send_files(['errors.x'])
            if baseworkdir in os.getcwd():
                remove_workdir(baseworkdir)
            else:
                os.system('rm errors.x')
            return None
        
        try:
            os.chdir(self.bto_dir)
            call([self.bto_blas, workdir + '/' +filename])
            os.chdir(workdir)
            cfiles = glob.glob('*.c')
            if(len(cfiles) == 1): 
                self.send_header1(1)
                self.send_files(cfiles)
            else:
                with open('errors.x', 'w') as f:
                    f.write('The BTO server was unable to compile and generate an output file')
                self.send_header1(1)
                self.send_files(['errors.x'])
        except:
            self.send_error('BTO server has had an error')
        remove_workdir(baseworkdir)

    
class BTO_Client(BaseClient):
        
    def submit_request(self, host, port, user, options,file1):
        files = [file1]
        BaseClient.submit_request(self, host, port, user, options, files)       
