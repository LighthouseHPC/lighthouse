#LibSVM code using the sklearn toolkit.
#This will calculate the recall score and be compared to the
from sklearn import svm
import pandas
from sklearn.cross_validation import train_test_split
from sklearn.metrics import recall_score, classification_report, confusion_matrix
import time

datafile = input("Enter your datafile: ")
data = pandas.read_csv(datafile)
ranger = len(data.T) - 1
X = data.iloc[:,0:ranger]
Y = data.iloc[:, ranger]
xtrain,xtest,ytrain,ytest = train_test_split(X,Y, test_size = 0.34)
clf = svm.SVC(kernel='rbf', C = 10.0)
clf.fit(xtrain,ytrain)
ypred = clf.predict(xtest)
print(recall_score(ytest, ypred, average = 'weighted'))
print(confusion_matrix(ytest,ypred))
labels = [0.0, 1.0, 2.0]
print(classification_report(ytest,ypred, labels))
print(time.clock())
