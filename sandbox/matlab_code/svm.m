% Load training matrix and class vector
load('training_matrix.mat');
load('class_vector.mat');

% take first 480 matrices and do k-fold cross-validation
disp('23 24 26 31');
tm = training_matrix(1:19680,:);
tm = [tm(:,23),tm(:,24),tm(:,26),tm(:,31)];
cv = class_vector(1:19680);

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

    % train SVM model over training instances
    svmModel = svmtrain(tm(trainIdx,:), cv(trainIdx), ...
                 'Autoscale',true, 'Showplot',false, 'Method','LS', ...
                 'BoxConstraint',500, 'Kernel_Function','rbf', 'RBF_Sigma',0.5);

    % test the SVM model using test instances
    pred = svmclassify(svmModel, tm(testIdx,:), 'Showplot',false);

    % evaluate and update performance object
    cp = classperf(cp, pred, testIdx);
end

% print accuracy
cp.CorrectRate

% print confusion matrix
% columns:actual, rows:predicted, last-row: unclassified instances
cp.CountingMatrix

% take last 400 matrices and do k-fold cross-validation
tm = training_matrix(19681:39360,:);
cv = class_vector(19681:39360);

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

    % train SVM model over training instances
    svmModel = svmtrain(tm(trainIdx,:), cv(trainIdx), ...
                 'Autoscale',true, 'Showplot',false, 'Method','LS', ...
                 'BoxConstraint',500, 'Kernel_Function','rbf', 'RBF_Sigma',0.5);

    % test the SVM model using test instances
    pred = svmclassify(svmModel, tm(testIdx,:), 'Showplot',false);

    % evaluate and update performance object
    cp = classperf(cp, pred, testIdx);
end

% print accuracy
cp.CorrectRate

% print confusion matrix
% columns:actual, rows:predicted, last-row: unclassified instances
cp.CountingMatrix

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear;
% Load training matrix and class vector
load('training_matrix.mat');
load('class_vector.mat');
disp('23 24 26 12 31');
% take first 480 matrices and do k-fold cross-validation
tm = training_matrix(1:19680,:);
tm = [tm(:,23),tm(:,24),tm(:,26),tm(:,12),tm(:,31)];
cv = class_vector(1:19680);

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

    % train SVM model over training instances
    svmModel = svmtrain(tm(trainIdx,:), cv(trainIdx), ...
                 'Autoscale',true, 'Showplot',false, 'Method','LS', ...
                 'BoxConstraint',500, 'Kernel_Function','rbf', 'RBF_Sigma',0.5);

    % test the SVM model using test instances
    pred = svmclassify(svmModel, tm(testIdx,:), 'Showplot',false);

    % evaluate and update performance object
    cp = classperf(cp, pred, testIdx);
end

% print accuracy
cp.CorrectRate

% print confusion matrix
% columns:actual, rows:predicted, last-row: unclassified instances
cp.CountingMatrix

% take last 400 matrices and do k-fold cross-validation
tm = training_matrix(19681:39360,:);
cv = class_vector(19681:39360);

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

    % train SVM model over training instances
    svmModel = svmtrain(tm(trainIdx,:), cv(trainIdx), ...
                 'Autoscale',true, 'Showplot',false, 'Method','LS', ...
                 'BoxConstraint',500, 'Kernel_Function','rbf', 'RBF_Sigma',0.5);

    % test the SVM model using test instances
    pred = svmclassify(svmModel, tm(testIdx,:), 'Showplot',false);

    % evaluate and update performance object
    cp = classperf(cp, pred, testIdx);
end

% print accuracy
cp.CorrectRate

% print confusion matrix
% columns:actual, rows:predicted, last-row: unclassified instances
cp.CountingMatrix

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear;
% Load training matrix and class vector
load('training_matrix.mat');
load('class_vector.mat');
disp('23 24 26 12 25 31');
% take first 480 matrices and do k-fold cross-validation
tm = training_matrix(1:19680,:);
tm = [tm(:,23),tm(:,24),tm(:,26),tm(:,12),tm(:,31)];
cv = class_vector(1:19680);

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

    % train SVM model over training instances
    svmModel = svmtrain(tm(trainIdx,:), cv(trainIdx), ...
                 'Autoscale',true, 'Showplot',false, 'Method','LS', ...
                 'BoxConstraint',500, 'Kernel_Function','rbf', 'RBF_Sigma',0.5);

    % test the SVM model using test instances
    pred = svmclassify(svmModel, tm(testIdx,:), 'Showplot',false);

    % evaluate and update performance object
    cp = classperf(cp, pred, testIdx);
end

% print accuracy
cp.CorrectRate

% print confusion matrix
% columns:actual, rows:predicted, last-row: unclassified instances
cp.CountingMatrix

% take last 400 matrices and do k-fold cross-validation
tm = training_matrix(19681:39360,:);
cv = class_vector(19681:39360);

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

    % train SVM model over training instances
    svmModel = svmtrain(tm(trainIdx,:), cv(trainIdx), ...
                 'Autoscale',true, 'Showplot',false, 'Method','LS', ...
                 'BoxConstraint',500, 'Kernel_Function','rbf', 'RBF_Sigma',0.5);

    % test the SVM model using test instances
    pred = svmclassify(svmModel, tm(testIdx,:), 'Showplot',false);

    % evaluate and update performance object
    cp = classperf(cp, pred, testIdx);
end

% print accuracy
cp.CorrectRate

% print confusion matrix
% columns:actual, rows:predicted, last-row: unclassified instances
cp.CountingMatrix

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear;

% Load training matrix and class vector
load('training_matrix.mat');
load('class_vector.mat');

disp('23 24 26 12 25 31');
% take first 480 matrices and do k-fold cross-validation
tm = training_matrix(1:19680,:);
tm = [tm(:,23),tm(:,24),tm(:,26),tm(:,12),tm(:,25),tm(:,31)];
cv = class_vector(1:19680);

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

    % train SVM model over training instances
    svmModel = svmtrain(tm(trainIdx,:), cv(trainIdx), ...
                 'Autoscale',true, 'Showplot',false, 'Method','LS', ...
                 'BoxConstraint',500, 'Kernel_Function','rbf', 'RBF_Sigma',0.5);

    % test the SVM model using test instances
    pred = svmclassify(svmModel, tm(testIdx,:), 'Showplot',false);

    % evaluate and update performance object
    cp = classperf(cp, pred, testIdx);
end

% print accuracy
cp.CorrectRate

% print confusion matrix
% columns:actual, rows:predicted, last-row: unclassified instances
cp.CountingMatrix

% take last 400 matrices and do k-fold cross-validation
tm = training_matrix(19681:39360,:);
cv = class_vector(19681:39360);

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

    % train SVM model over training instances
    svmModel = svmtrain(tm(trainIdx,:), cv(trainIdx), ...
                 'Autoscale',true, 'Showplot',false, 'Method','LS', ...
                 'BoxConstraint',500, 'Kernel_Function','rbf', 'RBF_Sigma',0.5);

    % test the SVM model using test instances
    pred = svmclassify(svmModel, tm(testIdx,:), 'Showplot',false);

    % evaluate and update performance object
    cp = classperf(cp, pred, testIdx);
end

% print accuracy
cp.CorrectRate

% print confusion matrix
% columns:actual, rows:predicted, last-row: unclassified instances
cp.CountingMatrix

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear;

% Load training matrix and class vector
load('training_matrix.mat');
load('class_vector.mat');

disp('23 24 26 12 25 1 31');
% take first 480 matrices and do k-fold cross-validation
tm = training_matrix(1:19680,:);
tm = [tm(:,23),tm(:,24),tm(:,26),tm(:,12),tm(:,25),tm(:,1),tm(:,31)];
cv = class_vector(1:19680);

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

    % train SVM model over training instances
    svmModel = svmtrain(tm(trainIdx,:), cv(trainIdx), ...
                 'Autoscale',true, 'Showplot',false, 'Method','LS', ...
                 'BoxConstraint',500, 'Kernel_Function','rbf', 'RBF_Sigma',0.5);

    % test the SVM model using test instances
    pred = svmclassify(svmModel, tm(testIdx,:), 'Showplot',false);

    % evaluate and update performance object
    cp = classperf(cp, pred, testIdx);
end

% print accuracy
cp.CorrectRate

% print confusion matrix
% columns:actual, rows:predicted, last-row: unclassified instances
cp.CountingMatrix

% take last 400 matrices and do k-fold cross-validation
tm = training_matrix(19681:39360,:);
cv = class_vector(19681:39360);

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

    % train SVM model over training instances
    svmModel = svmtrain(tm(trainIdx,:), cv(trainIdx), ...
                 'Autoscale',true, 'Showplot',false, 'Method','LS', ...
                 'BoxConstraint',500, 'Kernel_Function','rbf', 'RBF_Sigma',0.5);

    % test the SVM model using test instances
    pred = svmclassify(svmModel, tm(testIdx,:), 'Showplot',false);

    % evaluate and update performance object
    cp = classperf(cp, pred, testIdx);
end

% print accuracy
cp.CorrectRate

% print confusion matrix
% columns:actual, rows:predicted, last-row: unclassified instances
cp.CountingMatrix

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear;

% Load training matrix and class vector
load('training_matrix.mat');
load('class_vector.mat');

disp('23 24 26 12 25 1 2 31');
% take first 480 matrices and do k-fold cross-validation
tm = training_matrix(1:19680,:);
tm = [tm(:,23),tm(:,24),tm(:,26),tm(:,12),tm(:,25),tm(:,1),tm(:,2),tm(:,31)];
cv = class_vector(1:19680);

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

    % train SVM model over training instances
    svmModel = svmtrain(tm(trainIdx,:), cv(trainIdx), ...
                 'Autoscale',true, 'Showplot',false, 'Method','LS', ...
                 'BoxConstraint',500, 'Kernel_Function','rbf', 'RBF_Sigma',0.5);

    % test the SVM model using test instances
    pred = svmclassify(svmModel, tm(testIdx,:), 'Showplot',false);

    % evaluate and update performance object
    cp = classperf(cp, pred, testIdx);
end

% print accuracy
cp.CorrectRate

% print confusion matrix
% columns:actual, rows:predicted, last-row: unclassified instances
cp.CountingMatrix

% take last 400 matrices and do k-fold cross-validation
tm = training_matrix(19681:39360,:);
cv = class_vector(19681:39360);

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

    % train SVM model over training instances
    svmModel = svmtrain(tm(trainIdx,:), cv(trainIdx), ...
                 'Autoscale',true, 'Showplot',false, 'Method','LS', ...
                 'BoxConstraint',500, 'Kernel_Function','rbf', 'RBF_Sigma',0.5);

    % test the SVM model using test instances
    pred = svmclassify(svmModel, tm(testIdx,:), 'Showplot',false);

    % evaluate and update performance object
    cp = classperf(cp, pred, testIdx);
end

% print accuracy
cp.CorrectRate

% print confusion matrix
% columns:actual, rows:predicted, last-row: unclassified instances
cp.CountingMatrix

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear;

% Load training matrix and class vector
load('training_matrix.mat');
load('class_vector.mat');

disp('23 24 26 12 25 1 2 21 31');
% take first 480 matrices and do k-fold cross-validation
tm = training_matrix(1:19680,:);
tm = [tm(:,23),tm(:,24),tm(:,26),tm(:,12),tm(:,25),tm(:,1),tm(:,2),tm(:,21),tm(:,31)];
cv = class_vector(1:19680);

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

    % train SVM model over training instances
    svmModel = svmtrain(tm(trainIdx,:), cv(trainIdx), ...
                 'Autoscale',true, 'Showplot',false, 'Method','LS', ...
                 'BoxConstraint',500, 'Kernel_Function','rbf', 'RBF_Sigma',0.5);

    % test the SVM model using test instances
    pred = svmclassify(svmModel, tm(testIdx,:), 'Showplot',false);

    % evaluate and update performance object
    cp = classperf(cp, pred, testIdx);
end

% print accuracy
cp.CorrectRate

% print confusion matrix
% columns:actual, rows:predicted, last-row: unclassified instances
cp.CountingMatrix

% take last 400 matrices and do k-fold cross-validation
tm = training_matrix(19681:39360,:);
cv = class_vector(19681:39360);

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

    % train SVM model over training instances
    svmModel = svmtrain(tm(trainIdx,:), cv(trainIdx), ...
                 'Autoscale',true, 'Showplot',false, 'Method','LS', ...
                 'BoxConstraint',500, 'Kernel_Function','rbf', 'RBF_Sigma',0.5);

    % test the SVM model using test instances
    pred = svmclassify(svmModel, tm(testIdx,:), 'Showplot',false);

    % evaluate and update performance object
    cp = classperf(cp, pred, testIdx);
end

% print accuracy
cp.CorrectRate

% print confusion matrix
% columns:actual, rows:predicted, last-row: unclassified instances
cp.CountingMatrix

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear;

% Load training matrix and class vector
load('training_matrix.mat');
load('class_vector.mat');

disp('23 24 26 12 25 1 2 21 10 31');
% take first 480 matrices and do k-fold cross-validation
tm = training_matrix(1:19680,:);
tm = [tm(:,23),tm(:,24),tm(:,26),tm(:,12),tm(:,25),tm(:,1),tm(:,2),tm(:,21),tm(:,10),tm(:,31)];
cv = class_vector(1:19680);

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

    % train SVM model over training instances
    svmModel = svmtrain(tm(trainIdx,:), cv(trainIdx), ...
                 'Autoscale',true, 'Showplot',false, 'Method','LS', ...
                 'BoxConstraint',500, 'Kernel_Function','rbf', 'RBF_Sigma',0.5);

    % test the SVM model using test instances
    pred = svmclassify(svmModel, tm(testIdx,:), 'Showplot',false);

    % evaluate and update performance object
    cp = classperf(cp, pred, testIdx);
end

% print accuracy
cp.CorrectRate

% print confusion matrix
% columns:actual, rows:predicted, last-row: unclassified instances
cp.CountingMatrix

% take last 400 matrices and do k-fold cross-validation
tm = training_matrix(19681:39360,:);
cv = class_vector(19681:39360);

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

    % train SVM model over training instances
    svmModel = svmtrain(tm(trainIdx,:), cv(trainIdx), ...
                 'Autoscale',true, 'Showplot',false, 'Method','LS', ...
                 'BoxConstraint',500, 'Kernel_Function','rbf', 'RBF_Sigma',0.5);

    % test the SVM model using test instances
    pred = svmclassify(svmModel, tm(testIdx,:), 'Showplot',false);

    % evaluate and update performance object
    cp = classperf(cp, pred, testIdx);
end

% print accuracy
cp.CorrectRate

% print confusion matrix
% columns:actual, rows:predicted, last-row: unclassified instances
cp.CountingMatrix

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% >> svm: with tm = [tm(:,23),tm(:,31)];

% ans =

%     0.7708


% ans =

%          574        4246
%          265       14595
%            0           0


% ans =

%     0.8620


% ans =

%          456        2492
%          223       16509
%            0           0

% >> svm with tm = [tm(:,23),tm(:,24),tm(:,31)];

% ans =

%     0.7680


% ans =

%          575        4302
%          264       14539
%            0           0


% ans =

%     0.8634


% ans =

%          461        2470
%          218       16531
%            0           0

% >> svm
% 23 24 26 31

% ans =

%     0.7694


% ans =

%          577        4276
%          262       14565
%            0           0


% ans =

%     0.8638


% ans =

%          460        2461
%          219       16540
%            0           0

% 23 24 26 12 31

% ans =

%     0.7661


% ans =

%          576        4341
%          263       14500
%            0           0


% ans =

%     0.8651


% ans =

%          464        2439
%          215       16562
%            0           0

% 23 24 26 12 25 31

% ans =

%     0.7703


% ans =

%          574        4255
%          265       14586
%            0           0


% ans =

%     0.8641


% ans =

%          454        2450
%          225       16551
%            0           0

% 23 24 26 12 25 31

% ans =

%     0.7708


% ans =

%          572        4244
%          267       14597
%            0           0


% ans =

%     0.8637


% ans =

%          466        2469
%          213       16532
%            0           0

% 23 24 26 12 25 1 31

% ans =

%     0.7691


% ans =

%          604        4310
%          235       14531
%            0           0


% ans =

%     0.8644


% ans =

%          456        2446
%          223       16555
%            0           0

% 23 24 26 12 25 1 2 31

% ans =

%     0.8014


% ans =

%          630        3700
%          209       15141
%            0           0


% ans =

%     0.8629


% ans =

%          454        2474
%          225       16527
%            0           0

% 23 24 26 12 25 1 2 21 31

% ans =

%     0.8066


% ans =

%          635        3603
%          204       15238
%            0           0


% ans =

%     0.8636


% ans =

%          454        2460
%          225       16541
%            0           0

% 23 24 26 12 25 1 2 21 10 31

% ans =

%     0.8216


% ans =

%          610        3282
%          229       15559
%            0           0


% ans =

%     0.8639


% ans =

%          459        2458
%          220       16543
%            0           0

% >> 