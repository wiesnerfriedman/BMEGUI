function [lag,covval,numpair,weight] = calccovmom(distmat,val,lagbound)

% calccovmom - Calculate covariance using method of moments approach
%              (Mar 30,2011)
%
% [lag,covval,numpair,weight] = calccovmom(distmat,val,lagbound)
%
% INPUT:
%
% distmat(n by n): Diatance matrix
% val(n by 1): Value
% lagbound(1 by m): Bound of lag
%
% OUTPUT :
%
% lag(1 by m): Spatial lag
% covval(1 by m): Experimental covariance
% numpair(1 by m): Number of pairs used to calculate experimental covariance
% weight(1 by m): Weight for general least square covarinace model fit
%

lag = zeros(1,size(lagbound,2)-1);
covval = zeros(1,size(lagbound,2)-1);
numpair = zeros(1,size(lagbound,2)-1);
weight = zeros(1,size(lagbound,2)-1);

for i = 1:size(lagbound,2) - 1 
    [idxrow,idxcol] = find(lagbound(i) < distmat & distmat <= lagbound(i+1));
    distpair = distmat(lagbound(i) < distmat & distmat <= lagbound(i+1));

    idxuniquepair = find(idxrow>=idxcol);
    idxrow = idxrow(idxuniquepair);
    idxcol = idxcol(idxuniquepair);
    distpair = distpair(idxuniquepair);

    valhead = val(idxrow);
    valtail = val(idxcol);

    covpair = (valhead - mean(val)).*(valtail - mean(val));

    lag(i) = mean(distpair);
    covval(i) = mean(covpair);
    numpair(i) = size(valhead,1);
    weight(i) = 1/var(covpair);
end

lag = [0,lag];
covval = [var(val,1),covval];
numpair = [size(val,1),numpair];
weight = [1/var((val - mean(val)).^2),weight];

