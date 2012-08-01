#!/usr/bin/python
# ADIC client program
#
# usage is the same as for "adiC" with additional
# required option "--username", e.g.:
# adic_client.py --username=nobody -vd gradient f.c
#
import sys, glob, socket
from ADIC import ADIC_Client
from utils import uniq, get_username, include_files, get_server, string

adic = ADIC_Client()
username, args = get_username(sys.argv[1:])
options,files = adic.check_options(args)
reduce(lambda x, y: x+y,map(glob.glob, files),[])      # expand unix wildcards 
files = uniq(include_files(files,adic.LanguageClass))  # add include files
try:
    host,port = get_server("r_adic")
except socket.error:
    sys.exit(1)
else:
    print adic.submit_request(host,port,username,string.join(options),files)

