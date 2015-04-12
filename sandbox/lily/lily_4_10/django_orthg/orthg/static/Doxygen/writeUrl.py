import csv
import pandas as pd
import numpy as np
from sklearn.externals.six import StringIO  

routine=pd.read_csv('url.csv',sep=',')

name = routine['rt']
prc = routine['prc']


i=0
f= open('filename.csv',"w")
for index in name:
    print prc[i]
    f.write(str(prc[i])+str(name[i])+'.f'+'\n')
    i += 1

f.close()  
