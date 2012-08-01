#! /usr/bin/python
# AD server program
from SocketServer import *
from ADIC import ADIC_Server
from utils import get_scheduler, update_load, shutdown, daemonize, change_workdir, remove_workdir
import sys, os, time, signal, socket, string, getopt, shutil
global upd_pid

# default settings
server_host = os.environ["HOST"] 
current_dir = os.environ["PWD"]
server_port = 3450
server_type = "r_adic"
exe = os.path.join(os.environ["ADICSERVER_ADICPATH"],'bin',os.environ["ADIC_ARCH"],'adiC')
scheduler_host,scheduler_port,scheduler_client_port=get_scheduler()
basedir = current_dir
timeout = 60
stdin  = '/dev/null'
stdout = basedir + '/' + server_type + '_daemon_' + str(os.getpid()) + '.log'
stderr = stdout
verbose = 1
        
###########################################################################
class AdMixinHandler:

    def handle(self):
        signal.alarm(timeout)
        if verbose: print time.asctime(),"server connected from",self.client_address
        host,port = self.client_address
        self.req_id = host+"_"+str(port)+"_"+str(int(time.time())) # unique request id
        self.users = ['nobody']                   # allowed user names for adic
        self.do_all(exe)
        sys.stdout.flush()
        sys.stderr.flush()
            
class AdRequestHandler(AdMixinHandler,ADIC_Server,StreamRequestHandler):
    def __init__(self, request, client_address, server):
        ADIC_Server.__init__(self)
	os.environ["ADIC_ARCH"] = "linux"
        StreamRequestHandler.__init__(self,request, client_address, server)

class MyServer(ForkingTCPServer):    
    def handle_error(self, request, client_address):
        print time.asctime(),"failed to serve request from",client_address
        sys.stdout.flush()
        sys.stderr.flush()
        self.close_request(request)
        self.server_close()
        raise

###########################################################################
def my_alarm_signal_handler(signum, frame):
    if verbose: print time.asctime(),'timeout: pid',os.getpid()
    raise

def my_signal_handler(signum, frame):
    global upd_pid
    if verbose: print time.asctime(),'signal handler caught signal', signum
    if os.getpid()!=upd_pid:
        if verbose: print time.asctime(),'sending TERM signal to update_load process (',upd_pid,')'
        os.kill(upd_pid,signal.SIGTERM)
        if verbose: print time.asctime(),'sending shutdown message to scheduler and exit'
        shutdown(server_host,server_port,scheduler_host,scheduler_port)
    sys.exit(0)

###########################################################################
def mainloop():
    addr = (server_host,server_port)
    svr  = MyServer(addr,AdRequestHandler)
    if verbose:
        print time.asctime(),"server process started (",os.getpid(),")"
        print time.asctime(),"host="+server_host," port="+str(server_port)
        print time.asctime(),"work directory="+os.getcwd()
    global upd_pid   # remember pid to inform update_load process when server goes down
    upd_pid=update_load(server_host,server_port,scheduler_host,scheduler_port,server_type)
    if verbose: print time.asctime(),"update_load process started (",upd_pid,")"
    time.sleep(1)
    while 1:
        sys.stdout.flush()
        sys.stderr.flush()
        svr.handle_request()
###########################################################################
# get command line arguments
opts,args = getopt.getopt(sys.argv[1:],"h",["help","verbose","timeout=","basedir=",
                                            "stdout=","stderr=","server_type=",
                                            "scheduler_host=","scheduler_port=",
                                            "server_port="])
for o, a in opts:
    if o in ("-h", "--help"):
        print "help"
    if o=="--verbose":
        verbose = 1
    if o=="--timeout":
        timeout = int(a)
    if o=="--server_port":
        server_port = int(a)
    if o=="--scheduler_port":
        scheduler_port = int(a)
    if o=="--basedir":
        basedir = a
    if o=="--stdout":
        stdout = a
    if o=="--stderr":
        stderr = a
    if o=="--server_type":
        server_type = a
    if o=="--scheduler_host":
        scheduler_host = a
        
daemonize(stdin,stdout,stderr)
signal.signal(signal.SIGTERM, my_signal_handler)
signal.signal(signal.SIGQUIT, my_signal_handler)
signal.signal(signal.SIGINT, my_signal_handler)
signal.signal(signal.SIGALRM, my_alarm_signal_handler)
os.chdir(basedir)    # change to work directory
mainloop()

