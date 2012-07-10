#!/usr/bin/python
# test client for remote execution of xaifbooster
# "EXAMPLE_0.xaif" must be present in current directory
from XAIF import XAIF_Client
d = open("EXAMPLE_0.xaif","r")
str = d.read()
d.close
results = XAIF_Client().submit1(str)
for pair in results:
    filename, data = pair
    if filename=="X2B_OUT.xaif":
        print data
