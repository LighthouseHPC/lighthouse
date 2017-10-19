import numpy as np
import matplotlib.pyplot as plt

from sklearn.linear_model import LogisticRegression
from sklearn.naive_bayes import GaussianNB
from sklearn.ensemble import RandomForestClassifier
from sklearn.ensemble import VotingClassifier
from sklearn.metrics import classification_report, recall_score,confusion_matrix
from sklearn.cross_validation import train_test_split
import pandas
import time
clf1 = LogisticRegression(random_state = 0)
clf2 = RandomForestClassifier(random_state = 0)
clf3 = GaussianNB()
labels = ['good', 'fair', 'bad']
datafile = input("Enter your datafile: "
data = pandas.read_csv(datafile)
a = len(data.T) -1
x = data.iloc[:, 0:a]
y = data.iloc[:,a]
xtrain,xtest,ytrain,ytest = train_test_split(x,y,test_size = 0.34)
eclf = VotingClassifier(estimators=[('lr', clf1), ('rf', clf2), ('gnb', clf3)],
                        voting='hard')
eclf.fit(xtrain,ytrain)
predictions = eclf.predict(xtest)
#print(eclf.transform(x))
#print(predictions)
#print(y)
print(classification_report(ytest, predictions, labels))
#print(recall_score(y,predictions,average = 'weighted'))
print(time.clock())
