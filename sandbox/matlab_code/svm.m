% Load training matrix and class vector
load('training_matrix.mat');
load('class_vector.mat');

% Number of folds
k=2;

% get indices of k-fold cross-validation
cvFolds = crossvalind('Kfold', class_vector, k); 

% initialize performance tracker
cp = classperf(class_vector);                      

% for each fold
for i = 1:k
    % get indices of test instances                                  
    testIdx = (cvFolds == i); 
    % get indices of training instances
    trainIdx = ~testIdx;             

    % train SVM model over training instances
    svmModel = svmtrain(training_matrix(trainIdx,:), class_vector(trainIdx), ...
                 'Autoscale',true, 'Showplot',false, 'Method','LS', ...
                 'BoxConstraint',10, 'Kernel_Function','rbf', 'RBF_Sigma',0.1);

    % test the SVM model using test instances
    pred = svmclassify(svmModel, training_matrix(testIdx,:), 'Showplot',false);

    % evaluate and update performance object
    cp = classperf(cp, pred, testIdx);
    
    % print performance after every fold
    cp.CorrectRate
end

% print accuracy
cp.CorrectRate

% print confusion matrix
% columns:actual, rows:predicted, last-row: unclassified instances
cp.CountingMatrix