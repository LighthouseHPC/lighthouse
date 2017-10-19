# -*- coding: utf-8 -*-
"""
Created on Mon Jul 10 15:15:22 2017

@author: A
"""
#This code is for the reduced set #1



from sklearn import metrics 
from sklearn.cross_validation import train_test_split 
import matplotlib.pyplot as plt
import matplotlib as mpl
import pandas 
from sklearn.neighbors import KNeighborsClassifier 
import numpy as np
from sklearn.metrics import accuracy_score
import cv2
datafile = input("Enter your datafile: "
info = pandas.read_csv(datafile)
print(info.head())

n_neighbors = 10
X = info.iloc[:,0:9]
Y = info.iloc[:, 9]
X_train, X_test, Y_train, Y_test = train_test_split(X,Y)
Knn = KNeighborsClassifier(n_neighbors).fit(X_train, Y_train)
Y_predict_test = Knn.predict(X_test)
Y_predict_train = Knn.predict(X_train)
results_test = accuracy_score(Y_test, Y_predict_test)
print(results_test)
results = metrics.classification_report(Y_test, Y_predict_test, target_names = ['good', 'fair', 'bad'])
print(results)
