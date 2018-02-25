# -*- coding: utf-8 -*-
"""
Created on Mon Jul 17 10:23:41 2017

@author: A
"""

from sklearn.naive_bayes import GaussianNB
import pandas
import numpy as np
import matplotlib.pylab as plt
from sklearn.cross_validation import train_test_split
from sklearn.tree import DecisionTreeClassifier
from sklearn.metrics import classification_report
import sklearn.metrics
from sklearn import metrics
from sklearn.ensemble import ExtraTreesClassifier
from sklearn.metrics import recall_score
from sklearn.metrics import accuracy_score
import
from sklearn.model_selection import KFold

#Begin Code:



target_names = ['good', 'fair', 'bad']
datafile = input("Enter your datafile: ")
print(datafile)
target_names = ['good', 'bad', 'fair']
data = pandas.read_csv(datafile)
a = len(data.T) - 1 #Again doing this to avoid as much hard coding as possible. 
X = data.iloc[:,0:a]
Y = data.iloc[:, a] #Y is the last colmn, good, bad, fair
X_train, X_test, Y_train, Y_test = train_test_split(X,Y, test_size = .34)
gnb = GaussianNB()
gnb.fit(X_train,Y_train)
y_predict_test = gnb.predict(X_test)

print(y_predict_test)
result = accuracy_score(Y_test, y_predict_test)
print(result)
results = metrics.classification_report(Y_test, y_predict_test, target_names)
print(results)
print(time.clock())
