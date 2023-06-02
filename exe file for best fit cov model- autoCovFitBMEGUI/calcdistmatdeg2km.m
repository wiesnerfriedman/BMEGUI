function distmat = calcdistmatdeg2km(lonlat)

% calcdistmatdeg2km - Calculate distance matrix in km based on Lat/Lon
%                     (Dec 16,2010)
%
% distmat = calcdistmatdeg2km(lonlat)
%
% INPUT:
%
% lonlat(n by 2): Location informaton 
%                 1st column is longitude (x)
%                 2nd column is latitude (y)
%
% OUTPUT :
%
% distmat(n by n): Distance(km) matrix
%

nloc = size(lonlat,1);
distmat = zeros(nloc);

for i = 1:nloc
    dist = distance([lonlat(i,2),lonlat(i,1)],[lonlat(:,2),lonlat(:,1)]);
    distmat(:,i) = deg2km(dist);
end
