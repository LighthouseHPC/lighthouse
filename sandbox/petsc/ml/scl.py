#!/usr/bin/env python

import numpy as np, random
alldata = np.loadtxt('solvers_anamod_2.csv',dtype=np.float64,comments='%',skiprows=2, delimiter=',')

# Split into data (training), and target (test)
numrows, numcols = alldata.shape
numtestrows = int(0.2 * numrows)
testrows = random.sample(xrange(numrows), numtestrows)
testrows = []

data = np.empty([0,numcols], dtype=np.float64)
test = np.empty([0,numcols], dtype=np.float64)
i_all = 0
for row in alldata:
    if i_all == 0:
        i_all += 1
        continue
        
    #print row
    if i_all in testrows:
        test = np.vstack([test, row])
    else:
        data = np.vstack([data, row])
    i_all += 1



#labelmap = {'good':1,'bad':-1}
#tlist = []
#for label in  data[:,numcols-1].tolist():
#    tlist.append(labelmap[label])
#target = np.array(tlist)

target = data[:,numcols-1]
data = np.delete(data,np.s_[-1:],1)
#print data.shape, target.shape
#print data
#print target


#TODO: create target by extracting N records from data

from sklearn import datasets
from sklearn.naive_bayes import BernoulliNB

for a in range(0,11):
    alpha = float(a/10.0)
    nb = BernoulliNB(alpha=alpha)
    y_pred = nb.fit(data, target).predict(data)
    print("Number of mislabeled points out of a total %d points (alpha = %f): %d"
          % (data.shape[0],alpha,(target != y_pred).sum()))
