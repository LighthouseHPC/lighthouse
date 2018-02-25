#This is the potential implementation of the C4.5 algorithm using the scikit learn module.
#I am not sure if this is the right way to implement it but here we go

#Begin Code:
from sklearn import tree
from sklearn.cross_validation import cross_val_score
from sklearn.tree import DecisionTreeClassifier, export_graphviz
from sklearn.cross_validation import train_test_split
from  sklearn.metrics import classification_report
from sklearn.model_selection import KFold
import graphviz
import pandas
import pydotplus
import time
test_names = ['good', 'fair', 'bad']
clf = DecisionTreeClassifier(criterion= 'entropy')
datafile = input("Enter your datafile: ")
data = pandas.read_csv(datafile)
a = len(data.T) - 1
X = data.iloc[:, 0:a]
Y = data.iloc[:,a]
X = X.values
Y = Y.values
kf = KFold(n_splits = 10)
for train_index, test_index in kf.split(X):
    X_train, X_test = X[train_index], X[test_index]
    Y_train, Y_test = Y[train_index], Y[test_index]
blah = clf.fit(X_train,Y_train)
predict = clf.predict(X_test)
dot_data = tree.export_graphviz(clf, out_file=None)
graph = pydotplus.graph_from_dot_data(dot_data)
graph.write_pdf("c4_5.pdf")
print(classification_report(Y_test, predict, test_names))
print(time.clock())
