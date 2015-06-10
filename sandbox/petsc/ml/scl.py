import arff, numpy as np
dataset = arff.load(open('solvers_anamod_2.arff', 'rb'))
data = np.array(dataset['data'])

#TODO: create target by extracting N records from data

from sklearn import datasets
from sklearn.naive_bayes import GaussianNB
gnb = GaussianNB()
y_pred = gnb.fit(data, target).predict(data)
print("Number of mislabeled points out of a total %d points : %d"
       % (iris.data.shape[0],(iris.target != y_pred).sum()))
