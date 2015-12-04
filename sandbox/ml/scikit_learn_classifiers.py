#This file does classification using scikit : Gaussian NaiveBayes, Decision Tree, SVM, RF


from sklearn.naive_bayes import GaussianNB
from sklearn.ensemble import RandomForestClassifier
from sklearn import metrics, preprocessing
from sklearn import svm, naive_bayes, neighbors, tree
from sklearn.ensemble import AdaBoostClassifier
from sklearn import cross_validation
from sklearn.metrics import confusion_matrix
from sklearn.cross_validation import cross_val_score
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



#filename = 'petsc_anammod497_1.arff'
#filename = 'petsc_anammod497.arff'
#filename = 'structProp31J.arff'
filename = 'petsc_anamod_35.arff'

dataset = loadarff(open(filename,'r'))
target = np.array(dataset[0]['class'])
train = np.array(dataset[0][[
'lambda-max-by-magnitude-im',
'right-bandwidth',
'n-nonzero-diags',
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
'nnzeros',
'lambda-min-by-magnitude-im',
'diagonal-average',
'diagonal-dominance',
'dummy-rows',
'ritz-values-r',
'nnzlow',
'colours',
'lambda-max-by-im-part-re',
'col-variability',
'trace-abs',
'ritz-values-c',
'diag-zerostart',
'loband',
'positive-fraction',
'trace',
'min-nnzeros-per-row',
'diagonal-sign',
'row-variability',
'nrows',
'colour-offsets',
'n-colours',
'diagonal-variance',
'departure',
'dummy-rows-kind',
'kappa',
'sigma-max',
'colour-set-sizes',
'sigma-diag-dist',
'n-ritz-values',
'ellipse-ax',
'ellipse-ay',
'ellipse-cx',
'normF',
'normInf',
'lambda-max-by-real-part-im',
'lambda-max-by-real-part-re',
'nnzdia',
'avgdistfromdiag',
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
#PCA normalizes and whitens the data, which means that the data is now centered on both components with unit variance:
#print(np.round(X1_pca.mean(axis=0), decimals=5))
#print('@@@@@@@@@@@@@@@@@@@@@@@@@@')
#print(np.corrcoef(X1_pca.T))
#Percentage of variance explained by each of the selected components. k is not set then all 
#components are stored and the sum of explained variances is equal to 1.0
#print('pca.explained_variance_ratio_   ----->  ',pca.explained_variance_ratio_) # sum can be found by pca.explained_variance_ratio_.sum() = 1 always
#print('pca.components_', pca.components_)
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



print('--------------------------------------------------')
# call Gaussian Naive Bayesian class with default parameters
gnb = GaussianNB()

for i in range(0,10):
	for j in range(0,58):
		if (np.isnan(X2).any()):
			print('there exists atleast one nan---> remove........')
			if(X[i][j]=='NaN'):
				continue
				#print(i,j,X[i][j+1], ': I am NaN......')

		else:
			if (np.isfinite(X2).all()):
				continue
				print('No nan but not finite: Not Desired -----> ' )
#print(type(X2[1][1]),'chking.........') #numpy float64
print('Size : ', X.shape, Y.shape, Z.shape)
gnb.fit(X, Y)
#print(type(X2[1][1]),type(Y[4][1]),type(Z[1][1]))
result_gnb = gnb.predict(Z)
#print('GNB prediction : ---> ',result_gnb)
#print('actual ans: -->',test_class)
CM = []
CM = confusion_matrix(test_class,result_gnb) 
print("Confusion Matrix : ")
print(CM)
for i in range(0,no_test_instances):
	if(test_class[i]== result_gnb[i]):
		score_gnb+=1
print('Accuracy GNB : ======> ', round(((score_gnb/no_test_instances )*100),2),'%')
c = np.concatenate((X,Z), axis=0)
#print('should be 4648',c.shape)
print("With cross validation : ")
score = cross_val_score(gnb,X,Y, cv = 10, scoring = 'accuracy')
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
#print(X.shape,'1111111',Y.shape,'1111111',Z.shape)
Xrows, Xcols = X.shape
Zrows,Zcols = Z.shape

for i in range(0,Xrows):
	for j in range(0,Xcols):
		if(X[i][j].astype('str')=='NaN' or X[i][j].astype('str')=='nan'):
			ct1+=1
			print(X[i][j].astype('str'))
		if(np.isfinite(X[i][j])==False):
			X[i][j] = np.finfo(np.float32).max
#		print(i,j,X[i][j])
		#print(type(X[0][0]))
#print('ct1----->',ct1)

for i in range(0,Xrows):
	for j in range(0,Xcols):
		#print(X.shape[0],X.shape[1])
		if (X[i][j] >= np.finfo(np.float64).max or X[i][j] == 'NaN' or X[i][j] == 'nan'):
			print('step 1-----------')
			#if (np.isfinite(X).all()):
				#print("Finite......") #it is finite
		#print('Not in range')

		#print('------ X ------',X[i][j])
result_dtree = dtree.fit(X,Y).predict(Z)

CM = confusion_matrix(test_class,result_dtree) 
print("Confusion Matrix : ")
print(CM)

#result_dtree = dtree.fit(X[1:2000],Y[1:2000]).predict(Z)
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
rf = RandomForestClassifier(n_estimators = 20, n_jobs = 8)
rf.fit(X,Y)
result_rf = rf.predict(Z)
#print('X', len(X),len(Y),len(X1[train_size:dataset_size]))
#print('RF prediction : ---> ',result_rf )
#print('actual ans: -->',test_class)
CM = confusion_matrix(test_class,result_rf) 
print("Confusion Matrix : ")
print(CM)
for i in range(0,no_test_instances):
	if(test_class[i]== result_rf[i]):
		score_rf+=1
print('Accuracy RF: =====> ', round(((score_rf/no_test_instances )*100),2),'%')
print("With cross validation : ")
score = cross_val_score(dtree,X,Y, cv = 10, scoring = 'accuracy')
print(score)
print("Mean", round((score.mean() * 100),2) , "%"  )
print('--------------------------------------------------')
#print(len(X1),type(X1))


 #13
rows,cols = X.shape #13,68
training_data = []
'''
train_size = int(0.8 * dataset_size) # 10
for row_no in range(0,train_size): #for 0.8 dataset : splitting dataset into training set
	for col_no in range(0,cols): # for all cols : 68
		training_data = X[row_no][col_no]
print(type(training_data))
'''
#print(X[1][67]) #539.874
#result = rf.predict(test)



#output
"""
GNB prediction : --->  [b'good' b'good' b'good' ..., b'good' b'good' b'good']
actual ans: --> [b'bad' b'bad' b'bad' ..., b'bad' b'bad' b'bad']
Accuracy : ======>  68.75614552605704 <==========
--------------------------------------------------
SVM prediction: ->  [b'bad' b'bad' b'bad' ..., b'bad' b'bad' b'bad']
Accuracy : ========>   97.71386430678466 <==========
--------------------------------------------------
Decision tree prediction: ->  [b'bad' b'bad' b'bad' ..., b'bad' b'bad' b'bad']
Accuracy : =====>  97.71386430678466 <==========
--------------------------------------------------
X 16268 16268 4068
RF prediction : --->  [b'bad' b'bad' b'bad' ...,pw b'bad' b'bad' b'bad']
actual ans: --> [b'bad' b'bad' b'bad' ..., b'bad' b'bad' b'bad']
Accuracy : =====>  98.08259587020649 <==========
"""
# writing the actual class value in the main output file for comparison
#no_of_lines = len(out)
#print('No. of rows in class file : ',no_of_lines)

#writing.writerow(['Decision Tree'])
#writing.writerow(['Gaussian NB'])
#writing.writerow(['RF'])
#writing.writerow(['SVM'])
#writing.writerows([y_dtree])
#writing.writerows([y_gnb])
#writing.writerows([y_rf])
#writing.writerows([y_svm])


# for i in y_dtree:
# 	#print('train_size  -->',train_size, 'no_of_lines --->',no_of_lines,'out--->',out[0][0])
# 	#print('chk-->', out[0][i-train_size],y_dtree[i-train_size])
# 	#writer.writerow({'Actual class': out[0][i-train_size]})
# 	writer.writerow({'Decision Tree': y_dtree[i-train_size], 'Gaussian NB': y_gnb[i-train_size], 'RF' : y_rf[i-train_size], 'SVM' : y_svm[i-train_size],'Actual class': out[0][i-train_size] })
# 	#print("Y_dtree -->",y_dtree[1])
# 	#for j in range(train_size,10): # no. of lines = 10 
# 	#print(out[0][i])
# 	#writer.writerow({'Actual class': out[0][i] })
# 	if(y_dtree[i-train_size] == 2 and out[0][i-train_size] ==2):
# 		true_negative_dtree +=1 # true-
# 	elif(y_dtree[i-train_size] == 1 and out[0][i-train_size] == 1) :
# 		true_positive_dtree +=1 #true+
# 	elif(y_dtree[i-train_size] == 1 and out[0][i-train_size] ==2):
# 		true_negative_dtree +=1 #false+
# 	elif(y_dtree[i-train_size] == 2 and out[0][i-train_size] == 1) :
# 		false_negative_dtree +=1 # false-

# 	if(y_gnb[i-train_size] == 2 and out[0][i-train_size] ==2):
# 		true_negative_gnb +=1 # true-
# 	elif(y_gnb[i-train_size] == 1 and out[0][i-train_size] == 1) :
# 		true_positive_gnb +=1 #true+
# 	elif(y_gnb[i-train_size] == 1 and out[0][i-train_size] ==2):
# 		true_negative_gnb +=1 #false+
# 	elif(y_gnb[i-train_size] == 2 and out[0][i-train_size] == 1) :
# 		false_negative_gnb +=1 # false-

# 	if(y_svm[i-train_size] == 2 and out[0][i-train_size] ==2):
# 		true_negative_svm +=1 # true-
# 	elif(y_svm[i-train_size] == 1 and out[0][i-train_size] == 1) :
# 		true_positive_gnb +=1 #true+
# 	elif(y_svm[i-train_size] == 1 and out[0][i-train_size] ==2):
# 		true_negative_svm +=1 #false+
# 	elif(y_svm[i-train_size] == 2 and out[0][i-train_size] == 1) :
# 		false_negative_svm +=1 # false-

# 	if(y_rf[i-train_size] == 2 and out[0][i-train_size] ==2):
# 		true_negative_rf +=1 # true-
# 	elif(y_rf[i-train_size] == 1 and out[0][i-train_size] == 1) :
# 		true_positive_rf +=1 #true+
# 	elif(y_rf[i-train_size] == 1 and out[0][i-train_size] ==2):
# 		true_negative_rf +=1 #false+
# 	elif(y_rf[i-train_size] == 2 and out[0][i-train_size] == 1) :
# 		false_negative_rf +=1 # false-



# total_guess_dtree = true_positive_dtree + true_negative_dtree + false_positive_dtree + false_negative_dtree
# print('Accuracy : Decision tree: ',(true_positive_dtree + true_negative_dtree)/total_guess_dtree *100)


# total_guess_gnb = true_positive_gnb + true_negative_gnb + false_positive_gnb + false_negative_gnb
# print('Accuracy : GaussianNB: ',(true_positive_gnb + true_negative_gnb)/total_guess_gnb *100)


# total_guess_svm = true_positive_svm + true_negative_svm + false_positive_svm + false_negative_svm
# print('Accuracy : SVM: ',(true_positive_svm + true_negative_svm)/total_guess_svm *100)


# total_guess_rf = true_positive_rf + true_negative_rf + false_positive_rf + false_negative_rf
# print('Accuracy : RF: ',(true_positive_rf + true_negative_rf)/total_guess_rf *100)

# table = [[true_positive_gnb, '|', false_negative_gnb],[false_positive_gnb, ' |', true_negative_gnb]]

# print('------------')
# print('Confusion Matrix GaussianNB: ')
# print(tabulate(table))
# #print('------------')
# #print(true_positive_gnb, '|', false_negative_gnb)
# #print(false_positive_gnb, ' |', true_negative_gnb)
# #print('------------')

# #print('right & wrong count',right_guess_gnb,wrong_guess_gnb)
# #total_guess_gnb = right_guess_gnb + wrong_guess_gnb
# #print('Accuracy : GaussianNB: ',right_guess_gnb/total_guess_gnb *100)


'''
dataset = arff.load(open(filename, 'r'))
meta, data = loadarff(filename)
#print(dataset)
no_of_lines = len(meta)
#print(meta[1][1],type(meta))
#print(meta.take([1,2,3,4]))
#print(meta.take(range(0,4))) #same as aboveline # helps you to pick n no. of rows 
#print(meta.take([1]))
#print(meta.take[1,2,3,4])
indices = [1,2]
print('--->',meta[1][1],'<---',len(meta), meta.shape)
#meta[:, 1] = val
print('a', meta.shape)
print(np.take(meta,indices, axis = -1))
#print(np.take(meta, [[0, 1], [2, 3]]))
b = np.array(meta)
#for i in range(len(meta)):
#	print(b[i][68])
#print(type(meta),meta['class'])
metalist = meta.tolist()
#print(meta.ndim())
'''

"""
#do not delete
Output: 
Y[1][24]----------------------- [b'bad' b'bad' b'bad' ..., b'bad' b'bad' b'bad'] <class 'int'>
--------------------------------------------------
Size :  (16268, 24) (16268,) (4068, 24)
GNB prediction : --->  [b'good' b'good' b'good' ..., b'good' b'good' b'good']
actual ans: --> [b'bad' b'bad' b'bad' ..., b'bad' b'bad' b'bad']
Accuracy : ======>  68.75614552605704 <==========
--------------------------------------------------
SVM prediction: ->  [b'bad' b'bad' b'bad' ..., b'bad' b'bad' b'bad']
Accuracy : ========>   97.71386430678466 <==========
--------------------------------------------------
Decision tree prediction: ->  [b'bad' b'bad' b'bad' ..., b'bad' b'bad' b'bad']
Accuracy : =====>  97.6401179941003 <==========
--------------------------------------------------
X 16268 16268 4068
RF prediction : --->  [b'bad' b'bad' b'bad' ..., b'bad' b'bad' b'bad']
actual ans: --> [b'bad' b'bad' b'bad' ..., b'bad' b'bad' b'bad']
Accuracy : =====>  98.08259587020649 <==========
--------------------------------------------------
#do not delete
"""

"""
#output without applying PCA on 'structProp31J.arff' (31 properties : symmetric)

--------------------------------------------------
Size :  (31488, 30) (31488,) (7872, 30)
GNB prediction : --->  [b'2' b'2' b'2' ..., b'2' b'2' b'2']
actual ans: --> [b'2' b'2' b'2' ..., b'2' b'2' b'2']
Accuracy : ======>  45.26168699186992 <==========
--------------------------------------------------
SVM prediction: ->  [b'2' b'2' b'2' ..., b'2' b'2' b'2']
Accuracy : ========>   96.20172764227642 <==========
--------------------------------------------------
Decision tree prediction: ->  [b'2' b'2' b'2' ..., b'2' b'2' b'2']
Accuracy : =====>  96.20172764227642 <==========
--------------------------------------------------
X 31488 31488 7872
RF prediction : --->  [b'2' b'2' b'2' ..., b'2' b'2' b'2']
actual ans: --> [b'2' b'2' b'2' ..., b'2' b'2' b'2']
Accuracy : =====>  96.20172764227642 <==========
--------------------------------------------------

"""