from utils import get_username, r_listdir, path_clt2svr, path_svr2clt, uniq, include_files, get_server, change_workdir, remove_workdir
import socket,sys,os,os.path,time,signal,shutil
import string,re,getopt,glob

class BaseMessage:

###########################################################################
# name   : recv_receipt
# purpose: verify that a previously sent message is arrived 
# args   : -
# output : whatever has been received instead of receipt (e.g. error msg) 
# return : int 1 if receipt is received ("ok"), int 0 otherwise 
    def recv_receipt(self):
        try:
            r = self.rfile.readline()
        except:
            return 0
        if r=="ok\n":     
            return 1
        else:
            print r
            return 0
###########################################################################
# name   : send_receipt
# purpose: confirm previously received message by sending receipt
# args   : - 
# output : -
# return : -
    def send_receipt(self):
        try:
            self.wfile.write("ok\n")
        except IOError:
            print "can't send receipt"
###########################################################################
# name   : send_error
# purpose: if something went wrong, send error message instead of receipt
# args   : string error message 
# output : -
# return : -
    def send_error(self,errmsg="  error\n"):
        print errmsg
        try:
            self.wfile.write(errmsg)
        except:
            pass
###########################################################################
# name   : send_header
# purpose: send message header consisting of multiple lines
# args   : list 
# output : -
# return : -
    def send_header(self,h_list):
        for h in h_list:
            self.send_header1(h)
###########################################################################
# name   : send_header1
# purpose: send one line
# args   : item to be send: typically string or int
# output : -
# return : -
    def send_header1(self,h):
        self.wfile.write(str(h)+"\n")
        if not self.recv_receipt():
            print "error while sending header"
            raise IOError
###########################################################################
# name   : recv_header1
# purpose: receive one line
# args   : -
# output : -
# return : string
    def recv_header1(self):
        try:
            header = string.strip(self.rfile.readline())
            # raise exception if string is empty:
            if header=="":
                self.send_error("empty header. ")
                raise IOError
            else:
                self.send_receipt()
                return header
        except:
            self.send_error("error while receiving header. ")
            raise IOError
###########################################################################
# name   : send1
# purpose: send 1 file
# args   : string filename, string data
# output : -
# return : -
    def send1(self,filename,data):
        self.wfile.write(filename+" "+str(len(data))+"\n")
        self.wfile.write(data)
        if not self.recv_receipt():
            print "error while sending file",filename
            raise IOError
###########################################################################
# name   : recv1
# purpose: receive 1 file
# args   : -
# output : -
# return : (string filename, string data)
    def recv1(self):
        try:
            [filename,filesize] = string.split(self.rfile.readline())
            filesize = int(filesize)
            data = self.rfile.read(filesize)
        except ValueError, IOError:
            self.send_error("error while reading file info. ")
            raise IOError
        except:
            self.send_error("error while receiving data. ")
            raise IOError
        else:
            self.send_receipt()
            return (filename,data)
###########################################################################
# name   : read1
# purpose: read the contents of a file from disk
# args   : string filename
# output : -
# return : string data
    def read1(self,filename):
        d=open(filename,'r')
        data=d.read()
        d.close()
        return data
###########################################################################
# name   : write1 
# purpose: write the contents of a file to disk, possibly create new subdir
# args   : string filename, string data
# output : file is written to disk
# return : -
    def write1(self,filename,data):
        path,base = os.path.split(filename)
        if path and not os.path.exists(path):
            os.makedirs(path)
        d=open(filename,'w')
        d.write(data)
        d.close()
###########################################################################
# name   : send_files
# purpose: send multiple files
# args   : [string filenames]
# output : -
# return : -
    def send_files(self,list_of_files):
        for filename in list_of_files:
            try:
                data = self.read1(filename)
                self.send1(filename,data)
            except IOError:
                print "error while sending files"
                raise
            
###########################################################################
# name   : recv_files
# purpose: receive N files
# args   : int N
# output : received files are written to disk
# return : [string filenames]
    def recv_files(self,N):
        r_list = []
        for i in range(N):
            try:
                filename,data = self.recv1()
                self.write1(filename,data)
            except IOError:
                print "error while receiving files"
                raise
            else:
                r_list.append(filename)                
        return r_list
    
###########################################################################
class BaseServer(BaseMessage):
    """Base class for AD-servers"""
###########################################################################
# name   : check_user
# purpose: check if userid is valid
# args   : string userid
# output : - 
# return : string userid
    def check_user(self,userid):
        if not (userid in self.users):
# would be useful to let the user know that the userid is rejected.
# The server could send an error message instead of the "ok" receipt
# after receiving the first header line:
#            self.send_error(userid+" unknown user")
# currently it's only written to the server's log:
            print (userid+" unknown user")
            raise BaseException
        else:
            return userid
###########################################################################
# name   : check_options
# purpose: check command-line (options, path names)
# args   : string options
# output : -
# return : string options      
    def check_options(self,options):
        try:
            optlist, files = getopt.getopt(options.split(), self.legal_options, self.legal_longoptions)
        except getopt.GetoptError, e:
            bad = '-' + e.opt
            print "Bad option '%s' caught in check_options." %e.opt
            options = string.replace(options, bad, '')
            return self.check_options(options)

        files = map(path_clt2svr,files)
        options = []
        for o,a in optlist:
            options.append(string.strip(o+" "+a))
        return string.join(options+files)
###########################################################################
# name   : do_all
# purpose: general routine to handle one request
# args   : string executable, string request_id
# output : -
# return : -
    def do_all(self,exe):
        userid  = self.check_user(self.recv_header1())
        options = self.check_options(self.recv_header1())
        nfiles  = int(self.recv_header1())
        workdir = userid+"_"+self.req_id
        change_workdir(workdir)
        files = self.recv_files(nfiles)
        new_files = self.run_executable(exe+" "+options)
        self.send_header1(len(new_files))
        self.send_files(new_files)
        remove_workdir(workdir)
        
###########################################################################
# name   : run_executable
# purpose: execute command in subshell
# args   : string commandline
# output : current directory and command to be executed, stdout/stderr log files
# return : [new_files]       
    def run_executable(self,cmdline):
        oldfiles = r_listdir(".")
        out_log = file('stdout.log', 'w')
	err_log = file('stderr.log', 'w')
	print "["+str(os.getcwd())+"] "+cmdline
	child_stdin, child_stdout, child_stderr = os.popen3(cmdline)
	out_log.write(child_stdout.read())
	err_log.write(child_stdout.read())
	out_log.close()
	err_log.close()
	# which files are new?
	time.sleep(1)   #sometimes there are still nfs handles -> wait a second
	newfiles = r_listdir(".")
	return filter(lambda x: x not in oldfiles, newfiles)

###########################################################################
        
class BaseClient(BaseMessage):
    """Base class for AD-clients"""
###########################################################################
# name   : init_socket
# purpose: init socket and make 'rfile'/'wfile' objects
# args   : string host, int port
# output : - 
# return : -       
    def init_socket(self,host,port):
        try:
            s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
            if s.connect_ex((host, port))==0:
                self.rfile = s.makefile('rb', -1)
                self.wfile = s.makefile('wb', 0)
                s.close()
            else:
                raise socket.error
        except socket.error, e:
            print "socket error: ",e
            raise

###########################################################################
# name   : submit_request
# purpose: submit request to server, collect results
# args   : string host, int port, string user, string options, [filenames]
# output : received files are saved to disk
# return : [filenames]
    def submit_request(self,host,port,user,options,files):
        try:
            self.init_socket(host,port)
            self.send_header([user,options,len(files)])
            self.send_files(files)
        except:
            print "error while submitting request"
            sys.exit(1)
        # receive response
        try:
            nfiles = int(self.recv_header1())           # header (number of files)
            list_of_files = self.recv_files(nfiles)     # list of received files
        except:
            print "error while collecting results"
            sys.exit(1)
        else:
            return list_of_files
########################################################################

