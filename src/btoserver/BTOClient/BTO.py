import os, os.path
import glob, shutil
import re
import random
import time
import subprocess
from subprocess import call, check_output, CalledProcessError
from Base import BaseServer,BaseClient
from utils import change_workdir, remove_workdir

########################################################################
class BTO_Server(BaseServer):
    
    def __init__(self, btodir, u ,r , btoblas='./bin/btoblas'):
        self.legal_options = 'aecmt:r:s:p'
        self.legal_longoptions = ['precision=', 'empirical_off',
            'correctness', 'use_model', 'threshold=',
            'level1=', 'level2=', 'test_param=', 'search=',
            'ga_timelimit=', 'empirical_reps=', 'delete_tmp',
            'ga_popsize=', 'ga_nomaxfuse', 'ga_noglobalthread',
            'ga_exthread', 'partition_off']
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
        ### If it is locked, sleep random seconds before retrying acq.
        print "\nAcquiring lock on BTO..."
        cwd = os.getcwd()
        while os.path.exists('./bto.lock'):
            print "Busy, retrying in a few seconds."
            time.sleep(random.randint(5,15))
        if os.path.exists('./bto.lock') == False:
            open('./bto.lock', 'a').close(); # touch lock file
            print "Lock acquired."

        static_options = '--delete_tmp '
        os.chdir('/tmp')
        ###---- create userid+"_"+self.req_id and name it baseworkdir
        baseworkdir = self.users[0]+'_'+ self.req_id
        #print baseworkdir                  #salin_xx-xx-xx
        os.mkdir(baseworkdir)
        workdir = os.path.abspath(os.path.join('/tmp', baseworkdir))


        with open('/tmp/lighthouse_temp' + '/errors.x', 'w') as f:
            f.write('Failure')

        try:
            userid  = self.check_user(self.recv_header1())
            options = static_options + self.check_options(self.recv_header1())
            nfiles  = int(self.recv_header1())
        except Exception, e:
            print "Exception! ", e
            with open(baseworkdir+'/errors.x', 'w') as f:
                f.write('An error occurred while the BTO server was receiving the input file.')
            try:
                self.send_header1(1)
                self.send_files([baseworkdir+'/errors.x'])
                return None
            except IOError, e:
                print e.strerror
        else:
            ###---- set /tmp/userid+"_"+self.req_id to be workdir
            os.chdir(baseworkdir)

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
                # after detecting the relevant arg by regex
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
                print argv
                bto_out = check_output(argv, stderr=subprocess.STDOUT)
                #call(argv)

            except CalledProcessError, e:
                bto_out = e.output
                print "---Begin output from BTO---"
                print bto_out
                print "---END output from BTO ---"
                print " "
                bto_err = str(e)
                print bto_err
                #with open(basework
#                self.send_error("Failed to execute ./bin/btoblas %s" %workdir+ "/" +filename)
            else:
                print "---Begin output from BTO---"
                print bto_out
                print "---END output from BTO ---"
                ###---- go back to /tmp/salin_xx-xx-xx to check if the c files are generated
                os.chdir(workdir)
                cfiles = glob.glob('*.c')
                if(len(cfiles) == 1): 
                    print 'Sending C file.'
                    self.send_header1(1)
                    self.send_files(cfiles)
                else:
                    with open(workdir+'/errors.x', 'w') as f:
                        f.write('The BTO server was unable to compile and generate an output file.')
                        f.write('Generated call to bto:')
                        f.write(str(argv))
                        f.write(bto_out)
                    self.send_header1(1)
                    self.send_files([workdir+'/errors.x'])
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
        return BaseClient.submit_request(self, host, port, user, options, files)

