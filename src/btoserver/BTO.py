from Language import BTO
from Base import BaseServer,BaseClient
from utils import change_workdir, remove_workdir, path_clt2svr, path_svr2clt
import socket,sys,os,os.path,time,signal,shutil
import string,re,getopt

########################################################################

class ADIC:
    """Adic-specific functions and data for client and server"""
    def __init__(self):
        self.LanguageClass = C                    # language
        self.legal_options = "d:vatCi:I:kD:hgsu"  # allowed options
        self.legal_longoptions=[]                 # allowed options with long names

###########################################################################
# name   : read_script
# purpose: read specified section in ADIC init script
# args   : string filename, string section_name
# output : -
# return : string contents of specified section       
    def read_script(self,filename,section_name):
        d=open(filename,'r')
        text = d.read()
        d.close()
        # search for [<section_name>] and succeeding text
        section_pat = re.compile("\["+section_name+"\](?P<data>[.\w\s=-]*)",
                                 re.DOTALL | re.IGNORECASE | re.MULTILINE)
        section = section_pat.search(text)
        if section:
            return section.group('data')
        else:
            return ""

###########################################################################
class ADIC_Server(ADIC,BaseServer):
    """Adic-specific functions for the server"""
#    def __init__(self):
#        ADIC.__init__(self.rfile,wfile)
#        self.rfile = rfile
#        self.wfile = wfile
        
    def check_options(self,options):
        # similar to the Base class version, but additionally check/modify include path
        optlist,files = getopt.getopt(options.split(),self.legal_options,self.legal_longoptions)
        files = map(path_clt2svr,files)
        options = []
        for o,a in optlist:
            if o=="-I":
                options.append(string.strip(o+" "+path_clt2svr(a)))
            else:
                options.append(string.strip(o+" "+a))
        return string.join(options+files)            
###########################################################################
# name   : check_include
# purpose: apply 'transfcn' to include directives which occur in 'text'
# args   : string text, function (string->string) transfcn
# output : -
# return : string modified_text       
    def check_include(self,text,transfcn):
        c = self.LanguageClass(text)
        while 1:
            (start,end) = c.search_include()
            if start==-1: break
            str = c.text[start:end]
            c.replace(str,transfcn(str))
        return c.text

    def recv1(self):
        # new version of recv1: similar to the base version, but
        # with additional check/modification of path names.
        # Also check pathnames in include directives.
        try:
            [filename,filesize] = string.split(self.rfile.readline())
            filesize = int(filesize)
            filename = path_clt2svr(filename)
            data = self.check_include(self.rfile.read(filesize),path_clt2svr)
        except ValueError:
            self.send_error("error while reading file info ("+filename+")")
        except RuntimeError:
            self.send_error("bad filename in "+filename)
        except IOError:
            print "IOError"
        else:
            self.send_receipt()
            return (filename,data)
        
    def send1(self,filename,data):
        # like send1, but replace every "dotdot/" by "../" in path names
        filename = path_svr2clt(filename)
        data = self.check_include(data,path_svr2clt)
        self.wfile.write(filename+" "+str(len(data))+"\n")
        self.wfile.write(data)
        if not self.recv_receipt():
            print "error while sending file",filename
            raise IOError

class ADIC_Client(ADIC,BaseClient):
    """Adic-specific functions for the client"""
    def check_options(self,args):
        # files1 is the list of files that appear at the end of the command line.
        # The commandline, if legal, will be send to the AD server in the 'options' field. 
        # files2 (a superset of files1) may contain additional files to be transmitted
        # (e.g. header files, scripts), but they don't appear in the command line
        optlist,files1 = getopt.getopt(args,self.legal_options,self.legal_longoptions)
        files2=[]
        ret_options = []
        for o,a in optlist:
            if o=="-i":
                files2.append(a)
                ret_options.append(string.strip(o+" "+a))
                files2+=string.split(self.read_script(a,"SOURCE_FILES"))
            else:
                ret_options.append(string.strip(o+" "+a))
        return (ret_options+files1,files1+files2)        
    
