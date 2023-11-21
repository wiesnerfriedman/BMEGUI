function [c1,c2]=cartesian2riverProj(riverReaches,ch,pTolerance);
% cartesian2riverProj             - associate data points with a river network
% 
%
% SYNTAX:
%
% [c1,c2]=cartesian2riverProj(riverReaches,ch,pTolerance);
%
% INPUT:
% 
% riverReaches  cell array   Each cell of the array corresponds to a single
%                            reach in the river network (i.e. riverReaches{1}= [x,y] coordinates of
%                            reach 1).
% ch            np x 3       Set of xy coordinates of points for converting to 'river' coordinates
%                            np = total # of points in first set
%                            3rd column = time
% pTolerance    scalar       maximum distance between 'seed' points along the river network.
%                            Seed points are equidistant points (added at pTolerance intervals)
%                            added to the riverReaches cell array in order to 'snap' points that might 
%                            not be directly on the network.
%                            The [x,y] location of a point is modified to equal the [x,y] position of the
%                            closest seed point, if the original data point does not initially fall on
%                            given river network (and is known to be part of the network)

%
% OUTPUT:
%
% c1,c2         np x 5       Set of 'river' coordinates identifying a point location on a 
%                            reach by unique reach ID and distance to DS point
%                            column 1: X Position (in original units, i.e. lat/long)
%                            column 2: Y position
%                            column 3: Reach ID
%                            column 4: Length to DS End
%                            column 5: Time
%
% 
% NOTE: c1 is typically used as input into coord2distRiver.m

if nargin<3, pTolerance=0; end;

nr=length(riverReaches);
np1=size(ch,1);

xloc1=ch(:,1);
yloc1=ch(:,2);
%tloc1=ch(:,3);
idx=[];
pdist=[];
pmin=[];
ploc=[];

% identify the location of each x,y point by reach ID and calculate the distance to the DS point
c1=[ch(:,1:2) nan*zeros(np1,2) ch(:,3)];
for i=1:np1
  for ir=1:nr
    idx=find(xloc1(i) == riverReaches{ir}(:,1) & yloc1(i) == riverReaches{ir}(:,2));
    if ~isempty(idx)
      reachID=ir;
      distDS=sum(sqrt(diff(riverReaches{reachID}(1:idx,1)).^2 + diff(riverReaches{reachID}(1:idx,2)).^2));
      c1(i,3:4)=[reachID distDS];          
    end;    
  end;
end;

%Create seed points along all river reaches at distance=pTolerance
if pTolerance>0    
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
    lr=unique([l 0:pTolerance:l(end)]);
    xr=[interp1(l,x,lr)];
    yr=[interp1(l,y,lr)];
    newpoints=[xr' yr'];  
    riverReachesTemp{k}=newpoints;
  end;  
  %Calculate new River Point Matrix using seed points
  seedpoints=[];
  for m=1:length(riverReachesTemp)
    for n=1:size(riverReachesTemp{m},1)
      distDS=sum(sqrt(diff(riverReachesTemp{m}(1:n,1)).^2 + diff(riverReachesTemp{m}(1:n,2)).^2));
      seedpoints=[seedpoints;riverReachesTemp{m}(n,1) riverReachesTemp{m}(n,2) m distDS];
    end;
  end;
  %Snap points to the River Network if they were originally not part of the network
  idx2=find(isnan(c1(:,3)));
  if ~isempty(idx2)    
    for p=1:length(idx2)
    [D]=coord2dist(c1(idx2(p),1:2),seedpoints(:,1:2));
    D=D';
    [Y,I]=min(D);
    c1(idx2(p),1:4)=[seedpoints(I,1) seedpoints(I,2) seedpoints(I,3) seedpoints(I,4)];
    end;
  end;  
end;

%find if some points are not associated with a network
findnan=sum(isnan(c1(:,3)));
if findnan>0
  warning('Some points are not associated with the network, set pTolerance>0 and try again');
else
  disp('All Points are Now Associated with the Network');
end;


c2=c1;

  
    

