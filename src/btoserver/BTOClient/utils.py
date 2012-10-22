import sys, socket, time, signal, select
import os, os.path, getopt
import httplib
import string, re

###########################################################################
# name   : get_scheduler
# purpose: get scheduler info from web page
# args   : string URL that maintains scheduler info
# output : -
# return : (string host, int server_port, int client_port)
def get_scheduler(urlstring="www-unix.mcs.anl.gov/autodiff/adscheduler.html"):
    [hd,tl] = string.split(urlstring,"/",1)    
    conn = httplib.HTTPConnection(hd)
    conn.request("GET", "/"+tl)
    r = conn.getresponse()
    if r.status==200:
        for s in string.split(r.read()):
            if s[:5]=="HOST=": host=s[5:]
            if s[:12]=="SERVER_PORT=": svr_port=s[12:]
            if s[:12]=="CLIENT_PORT=": clt_port=s[12:]        
        return (host,int(svr_port),int(clt_port))
    else:
        print "Error:",r.status, r.reason

############################################################################
# name   : get_server
# purpose: get server info from scheduler
# args   : string server_type (e.g. "adic")
# output : -
# return : (string host, int port)
def get_server(server_type):
    host,svr_port,clt_port = get_scheduler()
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    if s.connect_ex((host, clt_port))==0:
        s.send("SERVER_TYPE:"+server_type+"\n")
        msg = string.split(s.recv(1024))
        if msg[0]=='NONE':
            print "no server of type "+server_type+" available"
            raise
        return (msg[0],int(msg[1]))
    else:
        print "no connection to scheduler",host,str(clt_port)
        raise socket.error
###########################################################################
# 'daemonize' function from python cookbook:
# http://aspn.activestate.com/ASPN/Cookbook/Python/Recipe/66012
def daemonize (stdin='/dev/null', stdout='/dev/null', stderr='/dev/null'):
    '''This forks the current process into a daemon.
    The stdin, stdout, and stderr arguments are file names that
    will be opened and be used to replace the standard file descriptors
    in sys.stdin, sys.stdout, and sys.stderr.
    These arguments are optional and default to /dev/null.
    Note that stderr is opened unbuffered, so
    if it shares a file with stdout then interleaved output
    may not appear in the order that you expect.
    '''
    # Do first fork.
    try: 
        pid = os.fork() 
        if pid > 0:
            sys.exit(0) # Exit first parent.
    except OSError, e: 
        sys.stderr.write ("fork #1 failed: (%d) %s\n" % (e.errno, e.strerror))
        sys.exit(1)
                    
    # Decouple from parent environment.
    os.chdir("/") 
    os.umask(0) 
    os.setsid() 
                
    # Do second fork.
    try: 
        pid = os.fork() 
        if pid > 0:
            sys.exit(0) # Exit second parent.
    except OSError, e: 
        sys.stderr.write ("fork #2 failed: (%d) %s\n" % (e.errno, e.strerror)    )
        sys.exit(1)
                    
    # Now I am a daemon!
        
    # Redirect standard file descriptors.
    si = file(stdin, 'r')
    so = file(stdout, 'a+')
    se = file(stderr, 'a+', 0)
    os.dup2(si.fileno(), sys.stdin.fileno())
    os.dup2(so.fileno(), sys.stdout.fileno())
    os.dup2(se.fileno(), sys.stderr.fileno())

###########################################################################
# name   : update_load
# purpose: called by server to inform scheduler about local cpu load
# args   : string server_host, int server_port, string scheduler_host, int scheduler_port, string server_type
# output : update load information sent to socket
# return : int pid 
def update_load(server_host,server_port,scheduler_host,scheduler_port,server_type):
    try: 
        pid = os.fork() 
        if not pid:
            while 1:
                try:
                    childin,childout = os.popen2('uptime')
                    load = string.split(childout.read(),',')[-2]
                    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
                    if s.connect_ex((scheduler_host,scheduler_port))==0:   #check if connection successful
                        s.send('SERVER_UPDATE\n')
                        s.send(server_host+" "+str(server_port)+" "+server_type+" "+str(load)+"\n")
                        s.send('SERVER_UPDATE_DONE\n')
                        s.close()
                    time.sleep(60)         # update load every 60 seconds 
                except:
                     break
            os._exit(0)
        else:
            return pid               
        
    except OSError, e: 
        sys.stderr.write ("fork failed: (%d) %s\n" % (e.errno, e.strerror)    )
        sys.exit(1)
        
###########################################################################
# name   : shutdown
# purpose: send shutdown message to scheduler
# args   : string server_host, int server_port, string scheduler_host, int scheduler_port
# output : to socket 
# return : -
def shutdown(server_host,server_port,scheduler_host,scheduler_port):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    if s.connect_ex((scheduler_host, scheduler_port))==0:
        s.send('SERVER_SHUTDOWN:'+server_host+':'+str(server_port)+'\n')
        s.close()

###########################################################################
# name   : get_username
# purpose: get username and remove it from argument list
# args   : [comand_line_arguments]
# output : -
# return : (string username,[comand_line_arguments])
def get_username(arglist):
    for a in arglist:
        x =re.match("--username=(?P<username>\S*)",a)
        if x:
            arglist.remove(a)
            return (x.group('username'),arglist)
    print "missing username"
    return ("",arglist)
###########################################################################
# name   : change_workdir 
# purpose: create and change work directory 
# args   : string workdir
# output : - 
# return : -
def change_workdir(wd):
    try:
        os.mkdir(wd)
        os.chdir(wd)
    except:
        print "cannot create local work directory:",wd
###########################################################################
# name   : remove_workdir 
# purpose: remove working dirctory and all files/subdirecories.
#          Note: argument must be the basename of current work directory 
# args   : string workdir (basename only)
# output : - 
# return : -
def remove_workdir(wd):
    if os.path.basename(os.getcwd())==wd:
        list = os.listdir(".")
        files = filter(os.path.isfile,list)
        dirs = filter(os.path.isdir,list)
        for i in files:
            os.remove(i)
        for i in dirs:
            os.chdir(i)
            remove_workdir(i)
        os.chdir("../")
        os.rmdir(wd)
    else:
        print "current directory is NOT",wd

###########################################################################
# name   : r_listdir
# purpose: recursive version of os.listdir, returns only 'real' files (no directories)
# args   : string path
# output : -
# return : [filenames]
def r_listdir(path):
    if path=="." or path=="./": prefix=""
    elif path[-1]!="/": prefix=path+"/"
    else: prefix=path
    list = map(lambda x: prefix+x,os.listdir(path))
    files = filter(os.path.isfile,list)
    dirs = filter(os.path.isdir,list)
    for i in dirs:
        files=files+r_listdir(i)
    return files

###########################################################################
# name   : include_files
# purpose: given a list of files, this function extends this list by include files,
#          if they exist. The LanguageClass must provide a method called
#          all_includefiles() returning such a list for a given piece of code.
# args   : [filenames], class LanguageClass
# output : -
# return : [filenames]
def include_files(files,LanguageClass):
    for filename in files:
        if os.path.isfile(filename):
            d=open(filename,"r")
            c = LanguageClass(d.read())
            d.close()
            new_filenames = c.all_includefiles()
            for nf in new_filenames:
                if not nf in files and os.path.isfile(nf):
#                    print "added",nf
                    files.append(nf)
    return files

###########################################################################
# name   : uniq
# purpose: removes duplicates from list
# args   : list
# output : -
# return : list
def uniq(alist):
    set = {}
    return [set.setdefault(e,e) for e in alist if e not in set]

###########################################################################
# name   : path_clt2svr and path_svr2clt
# purpose: functions for changing representations of path names:
#          ../ on client side corresponds to dotdot/ on server side.
#          Translation client->server also includes check for disallowed pathnames.
# args   : string path
# output : -
# return : string path
def path_clt2svr(path):
    # check for bad pathnames, translate "../" to "dotdot/"
    if string.strip(path[0]) in "~/":
        raise RuntimeError,"pathnames beginning with / or ~ are not allowed"
    else:
        return re.sub("\.\./", "dotdot/", path)

def path_svr2clt(path):
    # undo the changes performed by path_clt2svr
    return re.sub("dotdot/","../", path)

###########################################################################
