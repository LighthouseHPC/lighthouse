% For trying out the recommender system like the Netflix recommender 
% This would be used later for ranking solvers for user recommendations
% Date: February 14, 2018
% Source:: https://blogs.mathworks.com/loren/2015/04/22/the-netflix-prize-and-production-machine-learning-systems-an-insider-look/

movies = {'Big Hero 6','Birdman','Boyhood','Gone Girl','The LEGO Movie','Whiplash'};
users = {'Kevin','Jay','Ross','Spencer','Megan', 'Scott'};
ratings = [1.0, 4.0, 3.0, 2.0, NaN, 1.0;
           5.0, 1.0, 1.0, 4.0, 5.0, NaN;
           NaN, 2.0, 2.0, 5.0, 4.0, 5.0;
           5.0, NaN, 3.0, 5.0, 4.0, 4.0;
           3.0, 2.0, NaN, 3.0, 3.0, 3.0;
           4.0, 3.0, 3.0, NaN, 4.0, 4.0];


figure
subplot(2,1,1)
scatter(ratings(:,1),ratings(:,2),'filled')
lsline
xlim([0 6]); ylim([0 6])
title('Movie Preference Space by Two Users')
xlabel('Kevin''s ratings'); ylabel('Jay''s ratings')
for i = 1:size(ratings,1)
    text(ratings(i,1)+0.05,ratings(i,2),movies{i})
end
subplot(2,1,2)
scatter(ratings(:,1),ratings(:,4),'filled')
lsline
xlim([0 6]); ylim([0 6])
xlabel('Kevin''s ratings'); ylabel('Spencer''s ratings')
for i = 1:size(ratings,1)
    text(ratings(i,1)+0.05,ratings(i,4),movies{i})
end

sims = corr(ratings, 'rows', 'pairwise');
fprintf('Similarity between Kevin and Jay:     %.2f\n',sims(1,2))
fprintf('Similarity between Kevin and Spencer:  %.2f\n',sims(1,4))

sims = sims - eye(length(users)); % set self-correlations to 0
kevin_corrs = sims(1,:);
[ngh_corr, ngh_idx] = sort(kevin_corrs,'descend');

kevin_mu = nanmean(ratings(:,1));           % Kevin's average rating
ngh_corr(4:end) = [];                       % drop non-neighbors
ngh_idx(4:end) = [];                        % drop non-neighbors
ngh_mu = nanmean(ratings(:,ngh_idx),1);       % neighbor average ratings
Predicted = nan(length(movies),1);          % initialize an accumulator

for i = 1:length(movies)                    % loop over movies
    ngh_r = ratings(i,ngh_idx);             % neighbor ratings for the movie
    isRated = ~isnan(ngh_r);                % only use neighbors who rated
    meanCentered =...                       % mean centered weighted average
        (ngh_r(isRated) - ngh_mu(isRated)) * ngh_corr(isRated)'...
        / sum(ngh_corr(isRated));
    Predicted(i) = kevin_mu + meanCentered; % add Kevin's average
end

Actual = ratings(:,1);                      % Kevin's actual ratings
table(Actual, Predicted,'RowNames',movies)  % compare them to predicted

fprintf('Predicted rating for "%s": %.d\n',movies{3},round(Predicted(3)))