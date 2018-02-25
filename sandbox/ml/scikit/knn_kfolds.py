from sklearn import metrics
from sklearn.metrics import recall_score
from sklearn.cross_validation import train_test_split
#import matplotlib.pyplot as plt
#import matplotlib as mpl
import pandas
from sklearn.neighbors import KNeighborsClassifier
import numpy as np
import time
from sklearn.model_selection import StratifiedKFold


datafile = input("Enter your datafile: ")
data = pandas.read_csv(datafile)
#print(data.head())
#print(data.shape)
a = len(data.T) - 1
 #Because python goes from 0 to length - 1 and I wanted to classify based on how many columns there are, that is why I did the transformation and incorporated the -1.
n_neighbors = 10
X = data.iloc[:, 0:a]
Y = data.iloc[:, a]

x = X.values
y = Y.values
data = data.values
kf = StratifiedKFold(n_splits = 10)
for train_index, test_index in kf.split(x,y):
    xtrain, xtest = x[train_index], x[test_index]
    ytrain, ytest = y[train_index], y[test_index]

Knn = KNeighborsClassifier(n_neighbors=10).fit(xtrain, ytrain)
#predictionTest = Knn.predict(X_test)
#print(X_test)
#predict_Test = dataKnn.predict(X_test)
Y_predict_test = Knn.predict(xtest)
#Y_predict_train = Knn.predict(xtrain)
results = metrics.classification_report(ytest, Y_predict_test, target_names = ['good', 'fair', 'bad'])
#print(metrics.classification_report(Y_train, Y_predict_train, target_names = ['good', 'fair', 'bad']))
#with open("results.txt", "w") as f:
   # f.write("%s", % str(results))
print(results)
