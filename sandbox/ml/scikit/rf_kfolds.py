import pandas
import numpy as np
import matplotlib.pylab as plt
from sklearn.cross_validation import train_test_split
from sklearn.tree import DecisionTreeClassifier
from sklearn.metrics import classification_report
from sklearn import metrics
from sklearn import datasets
from sklearn.ensemble import ExtraTreesClassifier
from sklearn.metrics import recall_score
from sklearn.metrics import accuracy_score
from sklearn.ensemble import RandomForestClassifier
import time
from sklearn.model_selection import KFold


target_names = ['good', 'fair', 'bad']


#print(a)
#Begin actual Code:
datafile = input("Enter your datafile: ")
data = pandas.read_csv(datafile)
a = len(data.T) - 1
X = data.iloc[:,0:a] #the predictor class
#print(X)
Y = data.iloc[:,a] # The solutions
#print(Y)
X = X.values
Y = Y.values

kf = KFold(n_splits = 5)
for train_index, test_index in kf.split(X,Y):
    X_train, X_test = X[train_index], X[test_index]
    Y_train, Y_test = Y[train_index], Y[test_index]
classifier = RandomForestClassifier(n_estimators = 100)
classifier = classifier.fit(X_train, Y_train)
#print(classifier)
predictions = classifier.predict(X_test)
#result = recall_score(Y_test, predictions, average = 'weighted')
results = metrics.classification_report(Y_test, predictions, target_names)
print(results)
