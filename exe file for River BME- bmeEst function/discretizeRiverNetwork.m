function [riverGrid]=discretizeRiverNetwork(riverReaches,discreteDistance)
% discretizeRiverNetwork        - Creates a grid of equidistant points
%                                 along a network
%
%SYNTAX:
%[riverGrid]=discretizeRiverNetwork(riverReaches,discreteDistance)
%
%INPUT:
%
% riverReaches      1 x nr   cell  where nr=number of Reaches.  Each cell of the array corresponds to a single
%                            reach in the river network (i.e. riverReaches{1}= [x,y] coordinates of
%                            reach 1).
% 
% discreteDistance  scalar   maximum distance between grid points along the river network.
%                            grid points are equidistant points (added at discDistance intervals)
%                            added to the riverReaches cell array to create an equidistant grid of points
%                            along a given network
%                            
%OUTPUT:
%
% riverGrid         np x 2   same size as riverReaches, except contains equidistant points determined by 
%                            discDistance.  riverGrid can be used as input into 
%                            cartesian2riverProj.m to associate all points with the network before
%                            performing the BME estimation using a river
%                            metric

if nargin<2,discreteDistance=0.05;end;
if discreteDistance==0
  disp('Discretizing Distance set to 0, riverGrid = riverReaches, Set discreteDistance > 0 for equidistant points along the network');
end;

nr=length(riverReaches);

for k=1:nr
  xypoints=riverReaches{k};
  x=xypoints(:,1)';
  y=xypoints(:,2)';  
  l=[0 cumsum(sqrt(diff(x).^2+diff(y).^2))];
  if sum(diff(l)==0)>0      
    %disp('Some duplicate points were found and ignored');        
    idx=find(diff(l)==0);
    x(idx)=[];
    y(idx)=[];
    l=[0 cumsum(sqrt(diff(x).^2+diff(y).^2))];
  end;
  lr=[0:discreteDistance:l(end)];
  xr=[interp1(l,x,lr)];
  yr=[interp1(l,y,lr)];
  newpoints=[xr' yr'];  
  riverReachesTemp{k}=newpoints;
end; 
riverGridcell=riverReachesTemp;

%convert cell array to regular matrix
riverGrid=cell2matrix(riverGridcell);