load('pca_data_set.mat');

% Do PCA
[coeff,score,latent,tsquared,explained,mu] = pca(D);

% plot the eigenvalues
bar(latent)

% plot the 1st principal component
% bar(coeff(:,1))

% plot the 2nd principal component
% bar(coeff(:,2))

% plot the 3rd principal component
% bar(coeff(:,3))

% plot the 4th principal component
% bar(coeff(:,4))