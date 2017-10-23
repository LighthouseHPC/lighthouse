import numpy as np
import pandas 
from sklearn.cross_validation import train_test_split
from sklearn.tree import DecisionTreeClassifier
from sklearn.metrics import accuracy_score
from sklearn.metrics import classification_report 
from sklearn import tree
datafile = input("Enter your datafile: ")
data = pandas.read_csv(datafile)
a = len(data.T) - 1 #Because the indexing starts from zero!

X = data.iloc[:, range(0,a)]
#print(X)
Y = data.iloc[:,a]    
#print(Y)
X_train, X_test, Y_train, Y_test = train_test_split(X,Y, test_size = 0.34)
clf_gini = DecisionTreeClassifier(criterion = "gini")
clf_gini.fit(X_train, Y_train)  
#print(clf_gini)     
gini_pred = clf_gini.predict(X_test)
#print(gini_pred)
clf_entropy = DecisionTreeClassifier(criterion = 'entropy')
clf_entropy.fit(X_train, Y_train)
entropy_pred = clf_entropy.predict(X_test)
#print(entropy_pred)

target_names = ['good', 'fair', 'bad']
results = metrics.classification_report(Y_test, gini_pred, target_names)
print(results)
print(accuracy_score(Y_test, gini_pred))
