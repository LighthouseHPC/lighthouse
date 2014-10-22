% Load training matrix and class vector
loadParallelData;

% do k-fold cross-validation on 860 matrices
tm = D(1:23220,:);

% tm = [tm(:,1:15),tm(:,31)];
cv = Tc(1:23220);

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
                 'BoxConstraint',10, 'Kernel_Function','rbf', 'RBF_Sigma',0.01);
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