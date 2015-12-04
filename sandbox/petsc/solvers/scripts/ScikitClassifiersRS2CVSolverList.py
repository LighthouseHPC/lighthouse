#This file does classification using scikit for reduced features: Gaussian,Bernolli NaiveBayes, 
#Decision Tree, RF,knn,svm


from sklearn.naive_bayes import GaussianNB
from sklearn.ensemble import RandomForestClassifier
from sklearn import metrics, preprocessing
from sklearn import svm, naive_bayes, neighbors, tree
from sklearn.ensemble import AdaBoostClassifier
from sklearn.neighbors import NearestNeighbors
from sklearn.naive_bayes import GaussianNB , MultinomialNB, BernoulliNB
from sklearn import cross_validation
from sklearn.metrics import confusion_matrix
from sklearn.cross_validation import cross_val_score, ShuffleSplit, cross_val_predict
#from sklearn.cross_validation import cross_val_predict
#from sklearn.cross_validation import cross_val_predict
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
from numpy import array_str
import csv #not used right now 
import scipy as sp
import matplotlib.pyplot as plt

#6 features in RS2
filename = '../PETSc_FullFeatureSet/petsc_anamodRS2_35.arff'
dataset = loadarff(open(filename,'r'))
target = np.array(dataset[0]['class'])
train = np.array(dataset[0][[
'avg-diag-dist',
'norm1',
'col-variability',
'min-nnzeros-per-row',
'row-variability',
'kappa',
'solver'
]])


X1 = np.asarray(train.tolist(), dtype=np.str)
rows = X1.shape[0] #13
cols = X1.shape[1] #24
#print("X1",rows,cols)
#print(X1[0,:])
#print("hi",X1[:,68])
#dataMatrix = np.array(X1)   # Convert a list-of-lists into a numpy array.  aListOfLists is the data points in a regular list-of-lists type matrix.
pca = PCA(n_components = 2) #no. of components to be retained : 2 : class 1 ans class 2
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
print(X1.shape[1])
X1 = imp.fit_transform(X1) # sklearn.preprocessing.Imputer.fit_transform returns a new array, it doesn't alter the argument array
print(X1.shape[1])
imp.fit(X1)
#fit the data
pca.fit(X1)
#project the data along with the first two dimensions
X1_pca = pca.fit_transform(X1)

dataset_size = len(X1) #13
train_size = int(0.8 * dataset_size) #10

X = X1[0:train_size]
Y = target[0:train_size] #has the class for the training set i.e. good or bad for 3718 datapoints
#for confusion matrix
prediction = target[train_size:dataset_size] #	prediction of the test data
#print("prediction", prediction, len(prediction)) #actual class of the test datapoints


Z = X1[train_size:dataset_size]
Z = np.asarray(Z.tolist(), dtype=np.float64)

imp1 = Imputer(missing_values='NaN', strategy='median', axis=0)
X2 = imp1.fit_transform(Z) # sklearn.preprocessing.Imputer.fit_transform returns a new array, it doesn't alter the argument array
imp1.fit(Z)


test_class = target[train_size:dataset_size]
score_rf,score_gnb,score_dtree,score_svm = 0,0,0,0
no_test_instances = len(test_class)
imp = Imputer(missing_values='NaN', strategy='median', axis=0)
X2 = imp.fit_transform(X1) # sklearn.preprocessing.Imputer.fit_transform returns a new array, it doesn't alter the argument array
imp.fit(X2)

#for confusion matrix and comparison
true_positive_dtree,true_negative_dtree,false_positive_dtree,false_negative_dtree  = 0,0,0,0
true_positive_gnb,true_negative_gnb,false_positive_gnb,false_negative_gnb  = 0,0,0,0
true_positive_svm,true_negative_svm,false_positive_svm,false_negative_svm  = 0,0,0,0
true_positive_rf,true_negative_rf,false_positive_rf,false_negative_rf  = 0,0,0,0

solvers = []
solvers.append(X1[:,6])
#print("Solvers list : ", solvers, len(solvers),type(solvers),solvers[0][0])

n_neighbors = 10
h = .02
score_knn = 0
for weights in ['uniform', 'distance']: 
	classifierknn = neighbors.KNeighborsClassifier(n_neighbors, weights=weights)
	classifierknn.fit(X, Y)

nbrs = NearestNeighbors(n_neighbors=2, algorithm='ball_tree').fit(X)
result_knn = classifierknn.predict(Z)
#print("Knn --> ", result_knn,type(result_knn), result_knn.shape,test_class.shape)
for i in range(0,no_test_instances):
	if(test_class[i]== result_knn[i]):
		score_knn+=1
print('Accuracy Knn : ========>  ', round(((score_knn/no_test_instances )*100),2),'%')
print("With cross validation : ")
score_knn = cross_val_score(classifierknn,X,Y, cv = 10, scoring = 'accuracy')
print(score_knn)
print("Mean", round((score_knn.mean() * 100),2) , "%"  )
print('--------------------------------------------------')

#NB
classifierNB = GaussianNB()
classifierBNB = BernoulliNB()

score_NB = 0


result_NB = classifierNB.fit(X, Y).predict(Z)
CM = confusion_matrix(test_class,result_NB) 
print("Confusion Matrix : ")
print(CM)
#print('SVM prediction: -> ',result_svm)
for i in range(0,no_test_instances):
	if(test_class[i]== result_NB[i]):
		score_NB+=1
print('Accuracy NB : ========>  ', round(((score_NB/no_test_instances )*100),2),'%')
print("With cross validation : ")
score = cross_val_score(classifierNB,X,Y, cv = 10, scoring = 'accuracy')
print(score)
print("Mean", round((score.mean() * 100),2) , "%"  )
print('--------------------------------------------------')


score_NBB = 0
result_NBB = classifierBNB.fit(X, Y).predict(Z)
CM = confusion_matrix(test_class,result_NBB) 
print("Confusion Matrix : ")
print(CM)
#print('SVM prediction: -> ',result_svm)
for i in range(0,no_test_instances):
	if(test_class[i]== result_NBB[i]):
		score_NBB+=1
print('Accuracy Bernoulli NB : ========>  ', round(((score_NBB/no_test_instances )*100),2),'%')
print("With cross validation : ")
score = cross_val_score(classifierBNB,X,Y, cv = 10, scoring = 'accuracy')
print(score)
print("Mean", round((score.mean() * 100),2) , "%"  )
print('--------------------------------------------------')

#SVM
classifiersvm = svm.SVC()
#print('testing -->', trainTargets)
result_svm = classifiersvm.fit(X, Y).predict(Z)
CM = confusion_matrix(test_class,result_svm) 
print("Confusion Matrix : ")
print(CM)
#print('SVM prediction: -> ',result_svm)
for i in range(0,no_test_instances):
	if(test_class[i]== result_svm[i]):
		score_svm+=1
print('Accuracy SVM : ========>  ', round(((score_svm/no_test_instances )*100),2),'%')
print("With cross validation : ")
score = cross_val_score(classifiersvm,X,Y, cv = 10, scoring = 'accuracy')
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

result_dtree = dtree.fit(X,Y).predict(Z)

CM = confusion_matrix(test_class,result_dtree) 
print("Confusion Matrix : ")
print(CM)


#print('Decision tree prediction: -> ',result_dtree)
for i in range(0,no_test_instances):
	if(test_class[i]== result_dtree[i]):
		score_dtree+=1
print('Accuracy Decision Tree : =====> ', round(((score_dtree/no_test_instances )*100),2),'%')
print("With cross validation : ")
score = cross_val_score(dtree,X,Y, cv = 10, scoring = 'accuracy')
print(score)
print("Mean", round((score.mean() * 100),2) , "%"  )
print('--------------------------------------------------')


#Random Forests
rf = RandomForestClassifier(n_estimators = 100, n_jobs = 12, random_state = 4)
rf.fit(X,Y)
result_rf = rf.predict(Z)
#print(Z[70])
#print('X', len(X),len(Y),len(X1[train_size:dataset_size]))
#print('RF prediction : ---> ',result_rf )
#print('actual ans: -->',test_class)
CM = confusion_matrix(test_class,result_rf) 
print("Confusion Matrix : ")
print(CM)
for i in range(0,no_test_instances):
	if(test_class[i]== result_rf[i]):
		score_rf+=1
print("no_test_instances: ",no_test_instances, "RF score : ", score_rf)
print('Accuracy RF: =====> ', round(((score_rf/no_test_instances )*100),2),'%')
print("With cross validation : ")
#cv = ShuffleSplit(dataset_size, n_iter=3,test_size=0.1, random_state=0)

#score = cross_val_score(dtree,X,Y, cv = cv)
score = cross_val_score(rf,X,Y, cv = 10, scoring = 'accuracy')
print(score)
print("Mean", round((score.mean() * 100),2) , "%"  )
print('--------------------------------------------------')

predictn = cross_val_predict(rf,X1,target,cv = 10)
print("Check", X1.shape, target.shape)
print("Prediction : --> ",predictn,len(predictn))

#CM1 = confusion_matrix(Y_predicted,Y_actual) 
CM1 = confusion_matrix(predictn,target) 
print("Confusion Matrix : ")
print(CM1)
print("Good Solver Accuracy by RF : ",round((CM1[0][0]/(CM1[0][0] + CM1[1][0]) * 100 ),2) , " %")


good_good_count, good_bad_count, bad_good_count, bad_bad_count = 0,0,0,0
for i in range(0,len(predictn)):
	#print(target[i].decode('UTF-8'),type(target[i].decode('UTF-8')), predictn[i].decode('UTF-8'), type(predictn[i].decode('UTF-8')))
	if predictn[i].decode('UTF-8') == "good" and target[i].decode('UTF-8') == "good" :
		good_good_count+=1
		#print(predictn[i][solver].decode('UTF-8'))
	elif predictn[i].decode('UTF-8') == "bad" and target[i].decode('UTF-8') == "bad" :
		bad_bad_count+=1
	elif predictn[i].decode('UTF-8') == "good" and target[i].decode('UTF-8') == "bad" :
		good_bad_count+=1
	elif predictn[i].decode('UTF-8') == "bad" and target[i].decode('UTF-8') == "good" :
		bad_good_count+=1

	#print(target[i],type(target[i]))
print(good_good_count, " good % --> " , good_good_count/ (good_good_count + bad_good_count) )
print(bad_bad_count, " ", bad_good_count, " " , good_bad_count)



#predicting a single instance : 
test_file = '../PETSc_FullFeatureSet/TestInstance2.arff'
testset = loadarff(open(test_file,'r'))
#testset1 = np.array(testset[0])
#out = np.array(testset[0])
testset1 = np.array(testset[0][[
'avg-diag-dist',
'norm1',
'col-variability',
'min-nnzeros-per-row',
'row-variability',
'kappa',
'solver'
]])

print(type(Z),type(testset1),Z.shape)
#Z = X1[train_size:dataset_size]
#Z = np.asarray(Z.tolist(), dtype=np.float64)
out = np.array(testset[0]['class'])
out.resize(154,)

testset1 = np.asarray(testset1.tolist(), dtype=np.float64)
print("testset1 : ",testset1,testset1.shape)
print("-----------------------------XXXXXX--------------------------")

print(testset1[0,6])

ctr = 0
testset1.resize(154,7)
while ctr <=153:
	for i in range(0,153):
	
		testset1[i+1,:] = testset1[0,:]
		testset1[i,6] = solvers[0][i]
		ctr += 1
		out[i]= "good"
print("out --", out.shape,testset1.shape)
#print(testset1[:,8],testset1.shape)

rf = RandomForestClassifier(n_estimators = 100, n_jobs = 12, random_state = 4)
rf.fit(X1,target)
res_RF = rf.predict(testset1)
#res1 = cross_val_predict(rf,testset1,out)
#print("prediction --> ", res1)
#print(res1,type(res1),res1.shape)
good_solvers = []
bad_solvers = []
for i in range(res_RF.shape[0]):
	if res_RF[i].decode('UTF-8') == "good" : 
		good_solvers.append(int(testset1[i,6]))
	elif res_RF[i].decode('UTF-8') == "bad" : 
		bad_solvers.append(int(testset1[i,6]))

print("List of good solvers by RF  : " , good_solvers, len(good_solvers), len(bad_solvers))
print("-------------------------------------------------------")

dtree.fit(X1,target)
res_DT = dtree.predict(testset1)
good_solvers_DT = []
bad_solvers_DT = []
for i in range(res_DT.shape[0]):
	if res_DT[i].decode('UTF-8') == "good" : 
		good_solvers_DT.append(int(testset1[i,6]))
	elif res_DT[i].decode('UTF-8') == "bad" : 
		bad_solvers_DT.append(int(testset1[i,6]))

print("List of good solvers by DT : " , good_solvers_DT, len(good_solvers_DT), len(bad_solvers_DT))


classifiersvm.fit(X1,target)
res_SVM = classifiersvm.predict(testset1)
good_solvers_SVM = []
bad_solvers_SVM = []
for i in range(res_SVM.shape[0]):
	if res_SVM[i].decode('UTF-8') == "good" : 
		good_solvers_SVM.append(int(testset1[i,6]))
	elif res_SVM[i].decode('UTF-8') == "bad" : 
		bad_solvers_SVM.append(int(testset1[i,6]))

print("List of good solvers by SVM : " , good_solvers_SVM, len(good_solvers_SVM), len(bad_solvers_SVM))


classifierknn.fit(X1,target)
res_knn = classifierknn.predict(testset1)
good_solvers_knn = []
bad_solvers_knn = []
for i in range(res_knn.shape[0]):
	if res_knn[i].decode('UTF-8') == "good" : 
		good_solvers_knn.append(int(testset1[i,6]))
	elif res_knn[i].decode('UTF-8') == "bad" : 
		bad_solvers_knn.append(int(testset1[i,6]))

print("List of good solvers by knn : " , good_solvers_knn, len(good_solvers_knn), len(bad_solvers_knn))

classifierNB.fit(X1,target)
res_NB = classifierNB.predict(testset1)
good_solvers_NB = []
bad_solvers_NB = []
for i in range(res_NB.shape[0]):
	if res_NB[i].decode('UTF-8') == "good" : 
		good_solvers_NB.append(int(testset1[i,6]))
	elif res_NB[i].decode('UTF-8') == "bad" : 
		bad_solvers_NB.append(int(testset1[i,6]))

print("List of good solvers by NB : " , good_solvers_NB, len(good_solvers_NB), len(bad_solvers_NB))

classifierBNB.fit(X1,target)
res_NBB = classifierNB.predict(testset1)
good_solvers_NBB = []
bad_solvers_NBB = []
for i in range(res_NBB.shape[0]):
	if res_NBB[i].decode('UTF-8') == "good" : 
		good_solvers_NBB.append(int(testset1[i,6]))
	elif res_NBB[i].decode('UTF-8') == "bad" : 
		bad_solvers_NBB.append(int(testset1[i,6]))

print("List of good solvers by Bernoulli NB : " , good_solvers_NBB, len(good_solvers_NBB), len(bad_solvers_NBB))
