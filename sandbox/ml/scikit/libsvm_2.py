import pandas
from svmutil import *
from sklearn.metrics import classification_report
from sklearn.metrics import recall_score
from sklearn.cross_validation import  train_test_split
from sklearn.metrics import confusion_matrix
import numpy
from numpy import array
import time

datafile = input("Enter your datafile: ")
data = pandas.read_csv(datafile)
print(data)
train, test = train_test_split(data, test_size = 0.34)
print(type(train))
a = len(train.T) - 1
features = train.iloc[:, a]
print(type(features))
features = features.tolist()
train = train.values.tolist()
b = len(test.T) - 1
#print(test)
features_test = test.iloc[:, b]
features_test = features_test.tolist()
test = test.values.tolist()
param = svm_parameter()
param.kernel_type = RBF
param.C = 10
param.cross_validation = 0
param.nr_fold = 10
#print(len(features1))
#print(len(train))
#print(features1)
#print(train)
print(type(test))
print(type(features))
problem = svm_problem(features, train)
m = svm_train(problem, param)
print(m)
p_lbl, p_acc, p_prob = svm_predict(features_test,test,m)
print(recall_score(features_test, p_lbl, average = 'weighted'))
labels = [0.0, 1.0, 2.0]
print(len(p_lbl))

#print(type(p_lbl))
Mat = confusion_matrix(features_test, p_lbl)
print(Mat)
features_test = array(features_test)
p_lbl = array(p_lbl)
print(classification_report(features_test,p_lbl, labels))
print(time.clock())
