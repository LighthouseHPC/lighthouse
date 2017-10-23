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
import time
from sklearn.model_selection import KFold


#Begin Code:
datafile = input("Enter your datafile: ")
print(datafile)
target_names = ['good', 'bad', 'fair']
data = pandas.read_csv(datafile)
a = len(data.T) - 1 #Again doing this to avoid as much hard coding as possible.
X = data.iloc[:,0:a]
Y = data.iloc[:, a] #Y
X = X.values
Y = Y.values

kf = KFold(n_splits = 10)
for train_index, test_index in kf.split(X):
    X_train, X_test = X[train_index], X[test_index]
    Y_train, Y_test = Y[train_index], Y[test_index]
gnb = GaussianNB()
gnb.fit(X_train,Y_train)
y_predict_test = gnb.predict(X_test)

print(y_predict_test)
result = accuracy_score(Y_test, y_predict_test)
print(result)
results = metrics.classification_report(Y_test, y_predict_test, target_names)
print(results)
print(time.clock())
