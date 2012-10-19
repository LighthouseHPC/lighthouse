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
        ###---- created /tmp/userid+"_"+self.req_id and set it to be workdir
        baseworkdir = self.users[0]+'_'+ self.req_id
        #print baseworkdir                  #salin_17-47-53
	os.mkdir(baseworkdir)
        try:
            userid  = self.check_user(self.recv_header1())
            options = self.check_options(self.recv_header1())
	    nfiles  = int(self.recv_header1())
        except:
            with open(baseworkdir+'/errors.x', 'w') as f:
                f.write('An error occurred while the BTO server was receiving the input file: invalid userid or options.')
            self.send_header1(1)
            self.send_files([baseworkdir+'/errors.x'])
            return None
	else:
            os.chdir(baseworkdir)
            workdir = os.getcwd()
            #print workdir			#/tmp/salin_17-47-53
            files = self.recv_files(nfiles)
            filename = files[0]

	    ###---- change dir to bto/ in order to run ./bin/btiblas
            os.chdir(self.bto_dir)
            #print "Current folder is:", os.getcwd()			#/homes/salin/Lighthouse/BTOServer/bto
	    #print ".m file location:", workdir + '/' +filename 	#/tmp/salin_17-59-44/DGEM.m
            try:
            	call([self.bto_blas, workdir + '/' +filename])
            except:
            	self.send_error('Failed to execute ./bin/btoblas %s'%workdir+ '/' +filename)
	    else:
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

	finally:
	    ###---- delete /tmp/userid+"_"+self.req_id/ folder
            import shutil
            shutil.rmtree(workdir)
	    if os.path.exists(workdir) == False:
		print "%s is removed successfully!"%workdir
	    else:
		print "%s is not yet removed."%workdir


    
class BTO_Client(BaseClient):
        
    def submit_request(self, host, port, user, options,file1):
        files = [file1]
        BaseClient.submit_request(self, host, port, user, options, files)       
