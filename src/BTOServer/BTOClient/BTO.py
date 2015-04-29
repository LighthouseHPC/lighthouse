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
        self.legal_options = 'aecmt:r:s:pl:'
        self.legal_longoptions = ['precision=', 'empirical_off',
            'correctness', 'use_model', 'threshold=',
            'level1=', 'level2=', 'test_param=', 'search=',
            'ga_timelimit=', 'empirical_reps=', 'delete_tmp',
            'ga_popsize=', 'ga_nomaxfuse', 'ga_noglobalthread',
            'ga_exthread', 'partition_off', 'limit=']
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
        cwd = os.getcwd()
        static_options = '--delete_tmp '

        print "\nAcquiring lock on BTO..."
        acquire_lock(cwd)
        print "Lock acquired."

        filename = '' # of M file

        os.chdir('/tmp')
        ###---- create userid+"_"+self.req_id and name it baseworkdir
        baseworkdir = self.users[0]+'_'+ self.req_id
        os.mkdir(baseworkdir)
        workdir = os.path.abspath(os.path.join('/tmp', baseworkdir))


        # nested function
        def report_err(Errors, mfile = ''):
            os.chdir(workdir)
            fn = 'errors.x'
            print 'Generating errors file %s' %fn
            with open(fn, 'w') as f:
                f.write(Errors)
                f.write("Generated .m file used in call:\n")
                if mfile != '':
                    with open(mfile, 'r') as mf:
                        for line in mf:
                            f.write(line)

            try:
                self.send_header1(1)
                self.send_files([fn])
            except IOError, e:
                print str(e)

        # nested function
        def leave(pid = 0):
            clean_workdir(workdir)
            release_lock(cwd, pid)

        print "Workdir: %s" %workdir


        try:
            userid  = self.check_user(self.recv_header1())
            options = static_options + self.check_options(self.recv_header1())
            nfiles  = int(self.recv_header1())
        except Exception, e:
            errors = 'Exception raised while preparing for BTO call:\n    '
            errors = errors + str(e)
            print errors
            report_err(errors)
            leave()
            return None

        os.chdir(baseworkdir)

        try: # get .m file
            files = self.recv_files(nfiles)
            filename = files[0]
        except IOError, e:
            errors = "Exception raised in receiving .m file."
            errors = errors + e.strerror()
            print errors
            report_err(errors)
            leave()
            return None


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

        ###---- change dir to bto/ in order to execute ./bin/btoblas
        os.chdir(self.bto_dir)
        #print "Current folder is:", os.getcwd()                    #/homes/salin/Lighthouse/BTOServer/bto
        #print ".m file location:", workdir + '/' +filename         #/tmp/salin_xx-xx-xx/DGEM.m

        bto_out = ''
        ret = None
        pid = 0
        try:
            bto_stdout = open(workdir + '/stdout.txt', 'w')
            proc = subprocess.Popen(argv, stdout=bto_stdout, stderr=bto_stdout)
            proc.communicate()

            pid = proc.pid
            ret = proc.returncode
            with open(cwd + '/bto.lock', 'a') as pidfile:
                pidfile.write(str(proc.pid) + '\n')

        except OSError, e:
            bto_out = 'The BTO server was unable to compile and generate an output file.\n'
            bto_out = bto_out + 'Args: ' + str(argv) + '\n'
            bto_out = bto_out + 'Exception: ' + str(e) + '\n'
            bto_out = bto_out + 'Return status: %d' %proc.returncode
            bto_out = bto_out + '---BEGIN output from btoblas---\n'
            with open(workdir + '/stdout.txt', 'r') as f:
                for line in f:
                    bto_out = bto_out + line
            bto_out = bto_out + '--- END  output from btoblas---\n\n'

            bto_stdout.close()
            print bto_out
            report_err(bto_out, filename)
            leave(pid)
            return None

        else:
            with open(workdir + '/stdout.txt', 'r') as f:
                for line in f:
                    bto_out = bto_out + line
            bto_out = bto_out + 'Return status for %d: %d' %(pid, proc.returncode)
            bto_stdout.close()

        # with exit status normal
        print bto_out
        print str(argv)

        ###---- go back to /tmp/salin_xx-xx-xx to check if the c files are generated
        os.chdir(workdir)
        cfiles = glob.glob('*.c')
        if(len(cfiles) == 1):
            print 'Adding details to C file.'

            for line in reversed(open(filename).readlines()):
                prepend_line(cfiles[0], '// ' + line);
            prepend_line(cfiles[0], '// Using .m file:')
            prepend_line(cfiles[0], '// ' + str(argv))
            prepend_line(cfiles[0], '// Generated with args:')

            print 'Sending C file.'
            self.send_header1(1)
            self.send_files(cfiles)
        else:
            message = str(argv) + '\n'
            message = message + 'BTO failed to generate a unique C file, or exited '
            message = message + 'with error but return status 0.\n'
            message = message + "---BEGIN output from btoblas---\n"
            trunc = ''
            if(len(bto_out) > 1024*80): # bytes
                message = message + 'TRUNCATING LONG OUTPUT!\n'
                i = 0
                for line in bto_out:
                    trunc = trunc + line
                    i = i + 1
                    if(i > 128): #lines
                        break;
                message = message + 'TRUNCATING LONG OUTPUT!\n'
                message = message + trunc + '\n'
            else:
                message = message + bto_out
            message = message + "\n--- END  output from btoblas---\n\n"
            report_err(message, filename)

        leave(pid)
        return None;
    ### END bto_handle ###



# Helpers for bto_handle()
def prepend_line(filename, line):
    with open(filename, 'r+') as f:
        content = f.read()
        f.seek(0, 0)
        f.write(line.rstrip('\r\n') + '\n' + content)


def clean_workdir(workdir):
    ###---- delete the /tmp/userid+"_"+self.req_id/ folder
    shutil.rmtree(workdir)
    if os.path.exists(workdir) == False:
        print "%s is removed successfully!" %workdir
    else:
        print "%s is not yet removed." %workdir


def acquire_lock_old(cwd):
    os.chdir(cwd)
    while os.path.exists('./bto.lock'):
        randtime = random.randint(3,15)
        print "Busy, retrying in %d seconds." %randtime
        time.sleep(randtime)
    if os.path.exists('./bto.lock') == False:
        open('./bto.lock', 'w').close(); # touch lock file


def acquire_lock(cwd):
    os.chdir(cwd)
    pids = []
    ret = 0

    if os.path.exists('./bto.lock') == False:
        open('./bto.lock', 'w').close(); # touch lock file

    while ret == 0:
        with open('./bto.lock', 'r') as f:
            for line in f:
                pids.append(line)
                print line
        if pids == []:
            ret = 1

        for p in pids:
            print p
            check = os.popen("ps --pid %s" %p);
            out = check.read()
            ret = check.close()
            if(ret == 0): # if pid found to be running...
                if not "btoblas" in out:
                    print "Rogue pid %s" %p
                    continue # avoid non-bto matching procs
                randtime = random.randint(3,10)
                print "Busy, retrying in %d seconds." %randtime
                time.sleep(randtime)
                break     # ...take a 'break' (go check others)


def release_lock(cwd, pid = 0):
    print "Releasing lock..."
    os.chdir(cwd)
    if not os.path.exists('./bto.lock'):
        return
    else:
        if pid == 0:
            return 

        locktxt = []
        with open('./bto.lock', 'r') as f:
            for line in f:
                if not str(pid) in line:
                    locktxt.append(line)

        with open('./bto.lock', 'w') as f:
            for line in locktxt:
                f.write(line)
        print "Lock released."



class BTO_Client(BaseClient):

    def submit_request(self, host, port, user, options,file1):
        files = [file1]
        return BaseClient.submit_request(self, host, port, user, options, files)

