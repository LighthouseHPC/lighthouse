import csv
import pandas as pd
import numpy as np
from sklearn.externals.six import StringIO  

routine=pd.read_csv('url.csv',sep=',')

name = routine['rt']
prc = routine['prc']


i=0
f= open('filename.csv',"w")
for i in name:
    f.write(str(prc[i])+str(name[i])+'.f')
    i += 1

routine.close()
f.close()  
