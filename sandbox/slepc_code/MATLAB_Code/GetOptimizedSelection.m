function GetOptimizedSelection()
%UNTITLED Summary of this function goes here
%   Detailed explanation goes here
clc;

% real under 12 proc
%TestResults = importfile('Results.xlsx','Sheet1','D2:AA10627');
% complex under 12
TestResults = importfile('Results.xlsx','Sheet1','D2:AA15067');

%TestResults = importfile('Results.xlsx','Sheet1','D6:AA857');
%display(TestResults)
[m,n] = size(TestResults);
global featureCount;
featureCount=8;
input=TestResults(1,1:featureCount);
row=1;
m
%sizeM=0
resultRowCounter =1;
resultMatrix={};
for i= 1:m
    if(isequal(TestResults(i,1:featureCount),input)==0)
        %display('Optimizing')
        x =GetResult(A);
        if(size(x)>0)
            resultMatrix(resultRowCounter,:) = x;
            resultRowCounter=resultRowCounter+1;
            %[mA,nA]=size(A);
        end
        %sizeM=sizeM+mA;
        row=1;
        A={};
        input=TestResults(i,1:featureCount);
    end
    A(row,:) = TestResults(i,:);
    row=row+1;
end
x =GetResult(A);
if(size(x)>0)
    resultMatrix(resultRowCounter,:) = x;
end
%display(resultMatrix);


% SVM implementation
%svmStruct = svmtrain(cell2mat(resultMatrix(:,1:6)),resultMatrix(:,7),'showplot',true);
% svmStruct = multisvm(cell2mat(resultMatrix(:,1:featureCount)),resultMatrix(:,featureCount+1), cell2mat(resultMatrix(:,1:featureCount)))

catrogories =[false, true, false, true, true, false, false, true];
predNames = {'Size', 'Real?', 'Processors', 'Prob type', 'Spectrum', 'NoOfEigvalues', 'Tolerance', 'Diagonal'};
tc = ClassificationTree.fit(cell2mat(resultMatrix(:,1:featureCount)),resultMatrix(:,featureCount+1), 'CategoricalPredictors',catrogories , 'PredictorNames',predNames )
label = predict(tc, cell2mat(resultMatrix(:,1:featureCount)));
%xlswrite('Prediction.xls', horzcat(cell2mat(resultMatrix(:,1:featureCount)), label), 'Eigensolvers', 'E1');
%xlswrite('Optimum.xls', [resultMatrix label], 'Eigensolvers', 'E1');

view(tc,'mode','graph')
[E,SE,Nleaf,BestLevel] = cvLoss(tc)
%view(svmStruct,'mode','graph')
% expectedResult = resultMatrix(:,featureCount+1);
% svmResult = num2cell(svmStruct);
% 
% length = size(label);
% dt=0;
% svm=0;
% svmList=cell(length);
% for j=1:length
%     expectedString = strtrim(expectedResult(j));  
%     if(strcmp(expectedString,label(j))==0 ) 
%         dt=dt+1;
%     end
%     svmString={'arnoldi'};
%     if(isequal((svmResult(j)),{2}))
%         svmString={'gd'};
%     elseif(isequal((svmResult(j)),{3}))
%         svmString={'jd'};
%     elseif(isequal((svmResult(j)),{4}))
%         svmString={'krylovschur'};
%     elseif(isequal((svmResult(j)),{5}))
%         svmString={'power'};
%     elseif(isequal((svmResult(j)),{6}))
%         svmString={'NoConvergence'};
%     end
%     svmList(j)=svmString;
%     if(strcmp(expectedString,svmString) ==0) 
%         svm=svm+1;
%         %display([expectedString label(j) svmString]);
%     end
% end
% dt
% svm
% %display(horzcat(expectedResult,label,svmList ));
% display(dt/length(1,1))
% display(svm/length(1,1))
end

function [result] = multisvm(TrainingSet,GroupTrain,TestSet)
%Models a given training set with a corresponding group vector and 
%classifies a given test set using an SVM classifier according to a 
%one vs. all relation. 
%
%This code was written by Cody Neuburger cneuburg@fau.edu
%Florida Atlantic University, Florida USA
%This code was adapted and cleaned from Anand Mishra's multisvm function
%found at http://www.mathworks.com/matlabcentral/fileexchange/33170-multi-class-support-vector-machine/

u=unique(GroupTrain)
numClasses=length(u);
result = zeros(length(TestSet(:,1)),1);

%build models
for k=1:numClasses
    %Vectorized statement that binarizes Group
    %where 1 is the current class and 0 is all other classes
    
    G1vAll=(strcmp(GroupTrain, u(k)));
    models(k) = svmtrain(TrainingSet,G1vAll);
end

%classify test cases
for j=1:size(TestSet,1)
    for k=1:numClasses
        if(svmclassify(models(k),TestSet(j,:)))
            break;
        end
    end
    result(j) = k;
end
end

function [resultVec] = GetResult(A)

global featureCount;
inputVector = cell2mat(A(1,2:featureCount-1));
ASub= A(:,featureCount+1:end);  % test this fixed value

eigenCol = 5;
tolCol = 6;

% check number of converged eigenvalues
vec=cell2mat(ASub(:,2)); 
rm = find(vec < inputVector(eigenCol)); % change
ASub = removerows(ASub,'ind',rm);

% check residual with tolerance
vec=cell2mat(ASub(:,6)); 
rm = find(vec > inputVector(tolCol)); % change
ASub = removerows(ASub,'ind',rm);
[m,n] = size(ASub);

if(m>1)
    %check for time taken
    ASub = sortrows(ASub, 5);
    vec=cell2mat(ASub(:,5)); 
    rm = find(vec > 10* vec(1)); % if time taken is more than 10 times min time
    ASub = removerows(ASub,'ind',rm);
    
    [m,n] = size(ASub);
    if(m>1)
        % check for flops
        ASub = sortrows(ASub, 4);
        vec=cell2mat(ASub(:,4));
        rm = find(vec > 5* vec(1)); % if flops is more than 5 times min flops
        ASub = removerows(ASub,'ind',rm);
    end
    
    [m,n] = size(ASub);
    if(m>1)
        % check for iterations
        ASub = sortrows(ASub, 3);
        vec=cell2mat(ASub(:,3));
        rm = find(vec > 10* vec(1)); % if iterations is more than 10 times min iteration
        ASub = removerows(ASub,'ind',rm);
    end

% finally sort in terms of time taken and return the solver
ASub = sortrows(ASub, 4);
end
% worst case if no results
[m,n] = size(ASub);
if(m==0)
    %display('Worst case')
    ASub= A(:,featureCount+1:end);
    vec=cell2mat(ASub(:,2)); 
    rm = find(vec == 0); % change
    ASub = removerows(ASub,'ind',rm);
    ASub = sortrows(ASub, 2);
end
[m,n] = size(ASub);
if(m>0)
    resultVec = horzcat(A(1,1:featureCount),ASub(1,1));
%     if(m==1) 
%         resultVec
%     else
%         m
%     end
else
    %display('Skipping no results')
    resultVec = [];
    resultVec = horzcat(A(1,1:featureCount),'NoConvergence');
    
end
end