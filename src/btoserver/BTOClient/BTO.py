import os, os.path
import glob, shutil
import re
import random
import time
from subprocess import call
from Base import BaseServer,BaseClient
from utils import change_workdir, remove_workdir

########################################################################
class BTO_Server(BaseServer):
    
    def __init__(self, btodir, u ,r , btoblas='./bin/btoblas'):
        self.legal_options = 'ae:cmt:r:s:'
        self.legal_longoptions = ['precision=', 'empirical_off',
            'correctness', 'use_model', 'threshold=',
            'level1=', 'level2=', 'test_param=', 'search=',
            'ga_timelimit=', 'empirical_reps=', 'delete_tmp',
            'ga_popsize=', 'ga_nomaxfuse', 'ga_noglobalthread',
            'ga_exthread']
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
        ### Acquire lock on BTO in order to manage *.dot files and resource
        ### If it is locked, sleep rand[1,5] seconds before retrying acq.
        print "Acquiring lock on BTO..."
        cwd = os.getcwd()
        while os.path.exists('./bto.lock'):
            print "Busy, retrying in a few seconds."
            time.sleep(random.randint(1,5))
        if os.path.exists('./bto.lock') == False:
            open('./bto.lock', 'a').close(); # touch lock file
            print "Lock acquired."

        os.chdir('/tmp')
        ###---- create userid+"_"+self.req_id and name it baseworkdir
        baseworkdir = self.users[0]+'_'+ self.req_id
        #print baseworkdir                  #salin_xx-xx-xx
        os.mkdir(baseworkdir)
        try:
            print "check user"
            userid  = self.check_user(self.recv_header1())
            print "check options"
            options = self.check_options(self.recv_header1())
            print "receive n files"
            nfiles  = int(self.recv_header1())
        except:
            print "Exception!"
            with open(baseworkdir+'/errors.x', 'w') as f:
                f.write('An error occurred while the BTO server was receiving the input file.')
            self.send_header1(1)
            self.send_files([baseworkdir+'/errors.x'])
            return None
        else:
            ###---- set /tmp/userid+"_"+self.req_id to be workdir
            os.chdir(baseworkdir)
            workdir = os.getcwd()

            #print workdir                      #/tmp/salin_xx-xx-xx
            files = self.recv_files(nfiles)
            filename = files[0]

            ###---- change dir to bto/ in order to execute ./bin/btiblas
            os.chdir(self.bto_dir)
            #print "Current folder is:", os.getcwd()                    #/homes/salin/Lighthouse/BTOServer/bto
            #print ".m file location:", workdir + '/' +filename         #/tmp/salin_xx-xx-xx/DGEM.m
            try:
                # BTO's option makes it necessary to pass a 2 'word' arg
                # for --level1 or --level2, e.g.
                # --level1 "thread 2:12:2"
                # so we omit the space in our user supplied options (in templates.py)
                # in order to validate, then add the space here
                # after detecting by regex
                options_list = options.split()
                opt_list = [] #temporary
                for opt in options_list:
                    p = re.compile('^thread')
                    m = p.match(opt)
                    if m:
                        opt = opt[:6] + ' ' + opt[6:]

                    p = re.compile('^cache')
                    m = p.match(opt)
                    if m:
                        opt = opt[:5] + ' ' + opt[5:]

                    opt_list = opt_list + [opt]

                options_list = opt_list

                argv = [self.bto_blas, workdir + '/' + filename]
                argv = argv + options_list
                call(argv)

            except OSError, e:
                print e
                self.send_error("Failed to execute ./bin/btoblas %s" %workdir+ "/" +filename)
            else:
                ###---- go back to /tmp/salin_xx-xx-xx to check if the c files are generated
                os.chdir(workdir)
                cfiles = glob.glob('*.c')
                if(len(cfiles) == 1): 
                    self.send_header1(1)
                    self.send_files(cfiles)
                else:
                    with open(baseworkdir+'errors.x', 'w') as f:
                        f.write('The BTO server was unable to compile and generate an output file')
                    self.send_header1(1)
                    self.send_files([baseworkdir+'errors.x'])
        finally:
            ###---- delete the /tmp/userid+"_"+self.req_id/ folder
            shutil.rmtree(workdir)
            if os.path.exists(workdir) == False:
                print "%s is removed successfully!" %workdir
            else:
                print "%s is not yet removed." %workdir

            ### Clean up *.dot files generated by BTO
            os.chdir(self.bto_dir);
            for dotfile in glob.glob('*.dot'):
                os.remove(dotfile)

            ### Release lock
            print "Releasing lock..."
            os.chdir(cwd)
            if os.path.exists('./bto.lock') == False:
                print "Error in releasing lock on BTO (race condition)!"
            else:
                os.remove('./bto.lock');
                print "Lock released."

    
class BTO_Client(BaseClient):
        
    def submit_request(self, host, port, user, options,file1):
        files = [file1]
        BaseClient.submit_request(self, host, port, user, options, files)

