function [xydispGrid]=getXYGridData(x2,y2,pkR,kBME,kBMER)
%
%SYNTAX:
%[xydispGrid]=getXYGridData(x2,y2,pkR,kBMER)
%
%INPUT:
%x2,y2        n x 1       vector of x,y coordinates specifying location
%                         of extra grid data points
%
%pkR          np x 5      matrix of points used in BME estimation
%                         5 columns correspond to the 5 columns of the river
%                         point matrix
%
%kBME,kBMER   np x 1      BME estimates at locations pkR
%
%OUTPUT:
%xydispGrid   n x 4       matrix of new grid data points to add to existing grid data
%                         columns 1,2 are original xy positions from x2,y2
%                         column 3,4 is closest BME estimate, kBME (euclidian) or kBMER
%                         (river)
[xg2 yg2]=meshgrid(x2,y2);
xgrid=[];
ygrid=[];
for i=1:size(xg2,2)
  xgrid=[xgrid;xg2(:,i)];
  ygrid=[ygrid;yg2(:,i)];
end;
xygrid=[xgrid ygrid];

for i=1:length(xygrid)
  [D]=coord2dist(xygrid(i,1:2),pkR(:,1:2));
  D=D';
  [Y,I]=min(D);
  xydispGrid(i,1:4)=[xygrid(i,1:2) kBME(I) kBMER(I)];
end;


