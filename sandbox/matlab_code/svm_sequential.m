% Load training matrix and class vector for the sequential case
load('training_matrix.mat');
load('class_vector.mat');

% do k-fold cross-validation on 860 matrices
tm = training_matrix(1:35260,:);

tm = [tm(:,23),tm(:,24),tm(:,26),tm(:,2),tm(:,1), ...
	  tm(:,16),tm(:,18),tm(:,20),tm(:,14),tm(:,15), ...
	  tm(:,17),tm(:,19),tm(:,3),tm(:,12),tm(:,13),tm(:,31)];
cv = class_vector(1:35260);

% Number of folds
k=10;

% get indices of k-fold cross-validation
cvFolds = crossvalind('Kfold', cv, k); 

% initialize performance tracker
cp = classperf(cv);                      

% for each fold
for i = 1:k	
    % get indices of test instances                                  
    testIdx = (cvFolds == i); 
    % get indices of training instances
    trainIdx = ~testIdx;             
    trainStart = tic;
    % train SVM model over training instances
    svmModel = svmtrain(tm(trainIdx,:), cv(trainIdx), ...
                 'Autoscale',true, 'Showplot',false, 'Method','LS', ...
                 'BoxConstraint',500, 'Kernel_Function','rbf', 'RBF_Sigma',0.25);
    trainTime = toc(trainStart);
    testStart = tic;
    % test the SVM model using test instances
    pred = svmclassify(svmModel, tm(testIdx,:), 'Showplot',false);
    testTime = toc(testStart);
    % evaluate and update performance object
    cp = classperf(cp, pred, testIdx);
    disp('---------------');
    i
    trainTime
    testTime
    cp.CorrectRate
    disp('---------------');
end

% print accuracy
cp.CorrectRate

% print confusion matrix
% columns:actual, rows:predicted, last-row: unclassified instances
cp.CountingMatrix