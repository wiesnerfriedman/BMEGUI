function [lagbound,numpairs] = calclagbound(distmat,lagparam)

% calclagbound - Calculate lag bound for covariance calculation
%                (Dec 21,2010)
%
% lagbound = calclagbound(distmat,lagparam)
%
% INPUT:
%
% distmat(n by n): Diatance matrix
% lagparam(scalar): Size of lag bound
%
% OUTPUT :
%
% lagbound(1 by lagparam): Distance lag
% numpairs(1 by lagparam): Number of pairs in each lag
%                          First lag is 0
%

% MINPAIRSIZE = 30;

distmat = triu(distmat);
distlist = distmat(distmat > 0);
maxdist = max(distlist);
validdist = sort(distlist(distlist <= (maxdist / 2)));
deltalag = (maxdist / 2) * (1 / (lagparam - 1));
lagbound = 0:deltalag:(maxdist / 2);

numpairs = zeros(1,size(lagbound,2) - 1);
for i = 1:size(lagbound,2) - 1
    numpairs(i) = sum(validdist > lagbound(i) & validdist <= lagbound(i + 1));
%     if numpairs(i)< MINPAIRSIZE
%         warning(['Lag',num2str(i),' does not have enough pairs']);
%     end
end

numpairs = [size(distmat,1),numpairs];
