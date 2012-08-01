from Base import BaseServer,BaseClient
from utils import get_server, change_workdir, remove_workdir
import socket,sys,os,os.path,time,signal,shutil,string

########################################################################
class XAIF_Server(BaseServer):

    def __init__(self):
        self.users = ['XAIF']                     # allowed user names for xaifbooster
        self.legal_options = ""                   # allowed options    for xaifbooster
        self.legal_longoptions = []               # allowed options with long names for xaifbooster

class XAIF_Client(BaseClient):
###########################################################################
# name   : submit1
# purpose: submit one xaif-string to xaif-server and return results (without using files)
# args   : string data 
# output : -
# return : [(string filename, string data)] 
     def submit1(self,xaif_string):
         try:
             host,port = get_server("r_xaif")
             self.init_socket(host,port)
             # send message header
             user_id = "XAIF"
             filename = "default"
             options = "no_options "+filename
             self.send_header([user_id,options,"1"])
             # send data
             self.send1(filename,xaif_string)
         except:
            print "error while submitting request"
            sys.exit(1)
         try:
             # receive response 
             nfiles = int(self.recv_header1())
             r_list = []
             for i in range(0,nfiles):
                 f,d = self.recv1()
                 r_list.append((f,d))
         except:
             print "error while collecting results"
             sys.exit(1)
         else:
             return r_list
