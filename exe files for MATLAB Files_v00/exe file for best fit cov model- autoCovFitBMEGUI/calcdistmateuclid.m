function distmat = calcdistmateuclid(sptlcoord)

% calcdistmateuclid - Calculate euclidian distance matrix (Dec 16,2010)
%
% distmat = calcdistmateuclid(sptlcoord)
%
% INPUT:
%
% sptlcoord(n by 2): Location informaton 
%                    1st column is x
%                    2nd column is y
%
% OUTPUT :
%
% distmat(n by n): Distance matrix
%

nloc = size(sptlcoord,1);
distmat = zeros(nloc);

for i = 1:nloc
    distmat(:,i) = sqrt((sptlcoord(i,1)-sptlcoord(:,1)).^2 + ...
                        (sptlcoord(i,2)-sptlcoord(:,2)).^2);
end
