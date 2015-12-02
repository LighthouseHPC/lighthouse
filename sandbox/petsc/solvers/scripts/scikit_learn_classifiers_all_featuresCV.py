#This file does classification using scikit : Gaussian NaiveBayes, Bernoulli NaiveBayes, Decision Tree, SVM, RF


from sklearn.naive_bayes import GaussianNB , MultinomialNB, BernoulliNB
from sklearn.ensemble import RandomForestClassifier
from sklearn import metrics, preprocessing
from sklearn import svm, naive_bayes, neighbors, tree
from sklearn.neighbors import NearestNeighbors
from sklearn.ensemble import AdaBoostClassifier
from sklearn import cross_validation
from sklearn.metrics import confusion_matrix
from sklearn.cross_validation import cross_val_score,cross_val_predict
from sklearn import metrics
#from tabulate import tabulate
from scipy.io.arff import loadarff
from sklearn.preprocessing import Imputer
from arff import  *
from matplotlib.mlab import PCA # for PCA
from sklearn.decomposition import PCA
import arff
import pandas as pd
import numpy as np
import csv #not used right now 
import scipy as sp


#file with all features
filename = '../PETSc_FullFeatureSet/petsc_anamod_35.arff'

dataset = loadarff(open(filename,'r'))
target = np.array(dataset[0]['class'])
train = np.array(dataset[0][[
'lambda-max-by-magnitude-im',
'right-bandwidth',
'avgdistfromdiag',
'symmetry',
'n-dummy-rows',
'blocksize',
'max-nnzeros-per-row',
'diag-definite',
'avgnnzprow',
'lambda-max-by-magnitude-re',
'ellipse-cy',
'nnzup',
'ruhe75-bound',
'avg-diag-dist',
'nnz',
'lambda-min-by-magnitude-re',
'lambda-max-by-im-part-im',
'left-bandwidth',
'norm1',
'sigma-min',
'upband',
'n-struct-unsymm',
'lambda-min-by-magnitude-im',
'diagonal-average',
'diagonal-dominance',
'dummy-rows',
'ritz-values-r',
'symmetry-snorm',
'symmetry-fanorm',
'symmetry-fsnorm',
'colours',
'lambda-max-by-im-part-re',
'col-variability',
'trace-abs',
'ritz-values-c',
'nnzeros',
'diag-zerostart',
'loband',
'positive-fraction',
'trace',
'min-nnzeros-per-row',
'diagonal-sign',
'row-variability',
'nrows',
'lambda-max-by-real-part-im',
'colour-offsets',
'n-colours',
'relsymm',
'diagonal-variance',
'departure',
'nnzlow',
'n-nonzero-diags',
'sigma-max',
'dummy-rows-kind',
'kappa',
'n-ritz-values',
'colour-set-sizes',
'sigma-diag-dist',
'symmetry-anorm',
'ellipse-ax',
'ellipse-ay',
'ellipse-cx',
'normF',
'normInf',
'lee95-bound',
'lambda-max-by-real-part-re',
'nnzdia',
'trace-asquared',
'solver {89565283,8793455,90197667,49598909,91036839,45869639,45869638,45869637,47942867,89269802,89269803,89269801,89269804,59072883,59072882,59072881,7285381,7285384,59072884,49598911,49598910,49598912,30870721,36025723,36025722,53302993,30870720,64278029,36025724,44526267,8793454,8793456,8793453,17734818,32168839,32168838,57331597,95762352,57331599,57331598,32168837,88865078,88865079,49834417,49834419,49834418,88865076,88865077,11256942,11256943,11256941,42851841,11256944,31459546,17887723,91845162,53362206,95762355,12321508,75830644,57331600,43373444,95762353,43373441,43373442,43373443,91068411,91068410,5890861,85483012,5890863,5890862,18868444,18868441,18868443,18868442,91068408,47942864,47942865,47942866,91068409,8520536,5890860,82456576,29030069,95762354,90783920,99720138,29030071,29030070,29030072,85490469,30870723,30870722,26415435,26415434,26415433,26415432,13323659,45869640,36564233,68908713,7285382,36564232,7285383,75830645,36564234,81986705,29553941,29553943,29553942,69654761,29553944,69654763,69654762,32874609,32168840,90197664,90197665,90197666,69654760,37052870,37052871,19932321,19932323,19932322,19932324,80361466,80361467,80361464,80361465,49834420,1216556,38678404,38678401,38678402,38678403,32874611,32874610,32874612,44114477,44114476,44114479,44114478,36564235,36025721,75830647,75830646,85490471,85490470,85490472,18524981,37052869,37052868}' 
]])


X1 = np.asarray(train.tolist(), dtype=np.str)
rows = X1.shape[0] #13
cols = X1.shape[1] #24
#dataMatrix = np.array(X1)   # Convert a list-of-lists into a numpy array.  aListOfLists is the data points in a regular list-of-lists type matrix.
pca = PCA(n_components = 2) #no. of components to be retained : 2 : class 1 ans class 2
#myPCA = pca.fit(X1)
#myPCA = pca.fit_transform(X1)
#print('myPCA',myPCA)
ct2 = 0



#converting nan to NaN
for i in range(0,rows):
	for j in range(0,cols):
		#print('i, j, -->', i, j,X1[i][j])
		if (X1[i][j].astype('str') == 'nan'):
			ct2+=1
			X1[i][j] = X1[i][j].astype('str')
	#X[0][2].astype = 'str'
			X1[i][j] = 'NaN'  #replacing just fine.
imp = Imputer(missing_values='NaN', strategy='median', axis=0)
X1 = imp.fit_transform(X1) # sklearn.preprocessing.Imputer.fit_transform returns a new array, it doesn't alter the argument array
imp.fit(X1)

#fit the data
pca.fit(X1)
#project the data along with the first two dimensions
X1_pca = pca.fit_transform(X1)

dataset_size = len(X1) #13
train_size = int(0.8 * dataset_size) #10


"""
X2 = np.asarray(X.tolist(), dtype=np.float64)
print('X2.shape --->',X2.shape)
for i in range(0,train_size):
	for j in range(0,cols):
		if (X2[i][j] > np.finfo(np.float64).max):
			print('Not in range')
		else :
			print('In range........')
"""
X = X1[0:train_size]
Y = target[0:train_size]
#for confusion matrix
prediction = target[train_size:dataset_size]


Z = X1[train_size:dataset_size]
Z = np.asarray(Z.tolist(), dtype=np.float64)

imp1 = Imputer(missing_values='NaN', strategy='median', axis=0)
X2 = imp1.fit_transform(Z) # sklearn.preprocessing.Imputer.fit_transform returns a new array, it doesn't alter the argument array
imp1.fit(Z)


test_class = target[train_size:dataset_size]
score_rf,score_gnb,score_gnbB,score_dtree,score_svm = 0,0,0,0,0
no_test_instances = len(test_class)
imp = Imputer(missing_values='NaN', strategy='median', axis=0)
X2 = imp.fit_transform(X1) # sklearn.preprocessing.Imputer.fit_transform returns a new array, it doesn't alter the argument array
imp.fit(X2)

#for confusion matrix and comparison
true_positive_dtree,true_negative_dtree,false_positive_dtree,false_negative_dtree  = 0,0,0,0
true_positive_gnb,true_negative_gnb,false_positive_gnb,false_negative_gnb  = 0,0,0,0
true_positive_svm,true_negative_svm,false_positive_svm,false_negative_svm  = 0,0,0,0
true_positive_rf,true_negative_rf,false_positive_rf,false_negative_rf  = 0,0,0,0



print('--------------------------------------------------')
# call Gaussian Naive Bayesian class with default parameters
gnb = GaussianNB()

for i in range(0,10):
	for j in range(0,58):
		if (np.isnan(X2).any()):
			print('there exists atleast one nan---> remove........')
			if(X[i][j]=='NaN'):
				continue

		else:
			if (np.isfinite(X2).all()):
				continue
				print('No nan but not finite: Not Desired -----> ' )
print('Size : ', X.shape, Y.shape, Z.shape)

gnb_predictn = cross_val_predict(gnb,X1,target, cv = 10)
CM = confusion_matrix(target,gnb_predictn) 
print("n cross validation NB : ")
print(CM )
for i in range(0,len(X1)):
	if(target[i]== gnb_predictn[i]):
		score_gnb+=1
print('Accuracy GNB : ======> ', round(((score_gnb/len(X1) )*100),2),'%')
c = np.concatenate((X,Z), axis=0)
print("With cross validation : ")
score = cross_val_score(gnb,X1,target, cv = 10, scoring = 'accuracy')
print(score)
print("Mean", round((score.mean() * 100),2) , "%"  )
print('--------------------------------------------------')

#Bernoulli NB
gnbB = BernoulliNB()
gnbB_predictn = cross_val_predict(gnbB,X1,target, cv = 10)
CM_gnbB = confusion_matrix(target,gnbB_predictn) 
print("n cross validation Bernoulli NB :")
print( CM_gnbB )

score_gnbB = 0
#Multinomial NB
#Input X must be non-negative for MultinomialNB
for i in range(0,len(X1)):
	if(target[i]== gnbB_predictn[i]):
		score_gnbB+=1
print('Accuracy Bernoulli NB : ======> ', round(((score_gnbB/len(X1) )*100),2),'%')
c = np.concatenate((X,Z), axis=0)
print("With cross validation : ")
scoregnbB = cross_val_score(gnbB,X1,target, cv = 10, scoring = 'accuracy')
print(scoregnbB)
print("Mean", round((scoregnbB.mean() * 100),2) , "%"  )
print('--------------------------------------------------')


#SVM
classifiersvm = svm.SVC()
result_svm = cross_val_predict(classifiersvm,X1,target, cv = 10)
CM = confusion_matrix(target,result_svm) 
print("Confusion Matrix : ")
print(CM)
#print('SVM prediction: -> ',result_svm)
for i in range(0,len(X1)):
	if(target[i]== result_svm[i]):
		score_svm+=1
print('Accuracy SVM : ========>  ', round(((score_svm/len(X1) )*100),2),'%')
print("With cross validation : ")
score = cross_val_score(classifiersvm,X1,target, cv = 10, scoring = 'accuracy')
print(score)
print("Mean", round((score.mean() * 100),2) , "%"  )
print('--------------------------------------------------')

ct1 = 0
#Decision Tree
X= X.astype('float32', casting = 'same_kind')
dtree = tree.DecisionTreeClassifier()
Xrows, Xcols = X.shape
Zrows,Zcols = Z.shape

for i in range(0,Xrows):
	for j in range(0,Xcols):
		if(X[i][j].astype('str')=='NaN' or X[i][j].astype('str')=='nan'):
			ct1+=1
			print(X[i][j].astype('str'))
		if(np.isfinite(X[i][j])==False):
			X[i][j] = np.finfo(np.float32).max

for i in range(0,Xrows):
	for j in range(0,Xcols):
		#print(X.shape[0],X.shape[1])
		if (X[i][j] >= np.finfo(np.float64).max or X[i][j] == 'NaN' or X[i][j] == 'nan'):
			print('step 1-----------')
			#if (np.isfinite(X).all()):
				#print("Finite......") #it is finite
		#print('Not in range')

result_dtree = cross_val_predict(dtree,X1,target, cv = 10)

CM = confusion_matrix(target,result_dtree) 
print("Confusion Matrix : ")
print(CM)

#print('Decision tree prediction: -> ',result_dtree)
for i in range(0,len(X1)):
	if(target[i]== result_dtree[i]):
		score_dtree+=1
print('Accuracy Decision Tree : =====> ', round(((score_dtree/len(X1) )*100),2),'%')
print("With cross validation : ")
score = cross_val_score(dtree,X1,target, cv = 10, scoring = 'accuracy')
print(score)
print("Mean", round((score.mean() * 100),2) , "%"  )
print('--------------------------------------------------')


#Random Forests
rf = RandomForestClassifier(n_estimators = 20, n_jobs = 8)
result_rf = cross_val_predict(rf,X1,target, cv = 10)
#print('X', len(X),len(Y),len(X1[train_size:dataset_size]))
#print('RF prediction : ---> ',result_rf )
#print('actual ans: -->',test_class)
CM = confusion_matrix(target,result_rf) 
print("Confusion Matrix : ")
print(CM)
for i in range(0,len(X1)):
	if(target[i]== result_rf[i]):
		score_rf+=1
print('Accuracy RF: =====> ', round(((score_rf/len(X1) )*100),2),'%')
print("With cross validation : ")
score = cross_val_score(dtree,X1,target, cv = 10, scoring = 'accuracy')
print(score)
print("Mean", round((score.mean() * 100),2) , "%"  )
print('--------------------------------------------------')
#print(len(X1),type(X1))


#knn
n_neighbors = 10
score_knn = 0
for weights in ['uniform', 'distance']: 
	classifierknn = neighbors.KNeighborsClassifier(n_neighbors, weights=weights)
	#classifierknn.fit(X, Y)

nbrs = NearestNeighbors(n_neighbors=2, algorithm='ball_tree').fit(X)
result_knn = cross_val_predict(classifierknn,X1,target, cv = 10)
#print("Knn --> ", result_knn,type(result_knn), result_knn.shape,test_class.shape)
for i in range(0,len(X1)):
	if(target[i]== result_knn[i]):
		score_knn+=1
print('Accuracy Knn : ========>  ', round(((score_knn/len(X1) )*100),2),'%')
print("With cross validation : ")
score_knn = cross_val_score(classifierknn,X1,target, cv = 10, scoring = 'accuracy')
print(score_knn)
print("Mean", round((score_knn.mean() * 100),2) , "%"  )
print('--------------------------------------------------')


