#This is my attempt at the adtree algorithm
#for some reason, scikit learn's approach to ADtrees are adaboosting combined with the decision tree classifier

import numpy as np
import pandas
from sklearn.ensemble import AdaBoostClassifier
from sklearn.tree import DecisionTreeClassifier
from sklearn.model_selection import KFold
from sklearn import tree
import pydotplus
from sklearn.metrics import recall_score, classification_report
import time

datafile = input("Enter your datafile: ")
data = pandas.read_csv(datafile)
test_names = ['good', 'fair', 'bad']
clf = AdaBoostClassifier(DecisionTreeClassifier(criterion='gini', max_depth = 3))
a = len(data.T) - 1
x = data.iloc[:,0:a]
y = data.iloc[:,a]
x = x.values
y = y.values

kf = KFold(n_splits = 2)
for train_index, test_index in kf.split(x,y):
    xtrain, xtest = x[train_index], x[test_index]
    ytrain, ytest = y[train_index], y[test_index]
#print(xtrain)
#print(ytrain)
fitter = clf.fit(xtrain,ytrain)
ypred = clf.predict(xtest)
#dot_data = tree.export_graphviz(clf, out_file=None)
#graph = pydotplus.graph_from_dot_data(dot_data)
#graph.write_pdf("adtree.pdf")

print(classification_report(ytest, ypred, test_names))
print(time.clock())
#dot_data = tree.export_graphviz(clf, out_file=None)
#graph = pydotplus.graph_from_dot_data(dot_data)
#graph.write_pdf("ADTree.pdf")
