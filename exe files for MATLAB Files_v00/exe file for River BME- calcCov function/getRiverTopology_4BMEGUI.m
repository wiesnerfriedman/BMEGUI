function [riverReaches,riverTopology,infoval,infoMsg]=getRiverTopology_4BMEGUI(RiverNetwork)
% % % function [riverReaches,riverTopology,infoval,infoMsg]=getRiverTopology(riverReachesRaw,sRiverOutlet,...
% % %   distanceTolerance)

% getRiverTopology                - Generates River Topology (reach #, d/s reach, order, length)
%
% SYNTAX:
%
% [riverReaches,riverTopology,infoval,infoMsg]=getRiverTopology(riverReachesRaw,sRiverOutlet)
%
% INPUT:
%
% riverReachesRaw    cell array  x,y coordinates of river reaches
% sRiverOutlet       1 x 2       x,y coordianate of furthest pt. downstream(ocean outlet)
% distanceTolerance  scalar      distance tolerance to determine when two 
%                                points are co-located
%                                default is 0
%
% OUTPUT:
%
% riverReaches       cell array  x,y coordinates of river reaches ordered 
%                                from downstream to upstream end point
% riverTopology      nr x 4      Topology for each reach.  nr=# of reaches
%                                Column 1: Unique Reach ID
%                                Column 2: Order #
%                                Column 3: Downstream Reach ID
%                                Column 4: Total Reach Length
%
% infoval            scalar      Value corresponding to a particular warning message
%                                Can be 1-5
%
% infoMsg            string      Text string displaying warning message corresponding
%                                to infoval value
%
%%%%---------- Modified for BMEGUI: P Jat (May, 2011)---------------------
%--------- Convert '*.csv' riverNetwork to structured Network 
sRiverOutlet = RiverNetwork(end,:); % Get RiverOutlet (last line of RiverNetwork)
nanID = find(isnan(RiverNetwork(:,1)));

riverReachesRaw =[];
kk=1;
for ij=1:length(nanID)
    riverReach ={RiverNetwork(kk:nanID(ij)-1,:)};
    riverReachesRaw =[riverReachesRaw,riverReach];
    kk = nanID(ij)+1;
end
name='riverTopology';
distanceTolerance=0.5;
%%%%------------------ End :modification ----------------------------------


%%%%if nargin<4, name='riverTopology'; end;    % P Jat (blocked)
%%%%if nargin<3, distanceTolerance=0; end;     % P Jat


% check that riverReaches are river reaches, etc.
if size(sRiverOutlet,1)~=1 | size(sRiverOutlet,2)~=2, 
  error('sRiverOutlet must be of size 1 x 2'); 
end
xDS=sRiverOutlet(1,1);
yDS=sRiverOutlet(1,2);


% Get the endpoints of the raw river reaches
nr=length(riverReachesRaw);
for ir=1:nr
  riverReachesRawFirstNode(ir,1:2)=riverReachesRaw{ir}(1,:);
  riverReachesRawLastNode(ir,1:2)=riverReachesRaw{ir}(end,:);
end

% find the d/s point in the river reaches

infoval=0;
infoMsg=[];
idxReachOutlet=[];
nr=length(riverReachesRaw);
for ir=1:nr
  idx=find( xDS ==riverReachesRaw{ir}(:,1) & yDS ==riverReachesRaw{ir}(:,2) );
  if length(idx)>1
    infoval=1;
    infoMsg=sprintf('Found the river network downstream point more than once in reach %d',ir);
  elseif length(idx)==1
    if ~isempty(idxReachOutlet) 
      infoval=2;
      infoMsg=sprintf('Found the river network downstream point in reach %d and reach %d',ir,idxReachOutlet(1));
    else
      idxReachOutlet=[ir,idx];
    end
  end
end


% Check that the river downstream point is at one end of the reach, if not
% give an error msg
if isempty(idxReachOutlet)
  infoval=3;
  infoMsg='sRiverOutlet is a location not found in the river network';
end;
if sRiverOutlet~=riverReachesRaw{idxReachOutlet(1,1)}(1,:)& sRiverOutlet~=riverReachesRaw{idxReachOutlet(1,1)}(end,:)
  infoval=4;
  infoMsg=sprintf('The River Downstream point,sRiverOutlet, is not at one end of the reach. The point is currently located in element %d of reach %d',idx,ir);
end;
irRawDS=idxReachOutlet(1,1);

if infoval>0
  disp('Warning in getRiverTopology :  Cannot get the river topology because of the following reason :');
  disp(['  ' infoMsg]);
  riverReaches=[];
  riverTopology=[];
  return
end

% If needed, re-order d/s reach so that river network d/s point is at the
% beginning

ir=1;
riverOrder=1;
if idxReachOutlet(1,2)>1
  riverReaches{1}=riverReachesRaw{irRawDS}(end:-1:1,:);
else
  riverReaches{1}=riverReachesRaw{irRawDS};
end;
riverReachesRaw(irRawDS)=[];
riverReachesRawFirstNode(irRawDS,:)=[];
riverReachesRawLastNode(irRawDS,:)=[];
riverTopology(1,1:3)=[ir riverOrder 0];

networkInterupted=0;
finishedWithPreviousOrder=1;
% Go through all the river reach network
while length(riverReachesRaw)>0 & networkInterupted==0
  if finishedWithPreviousOrder==1
    riverOrder=riverOrder+1;
  end
  idxPreviousOrder=find(riverTopology(:,2)==riverOrder-1); % index to reaches of previous order
  if length(idxPreviousOrder)==0
    riverTopology
    networkInterupted=1;
    infoval=5;
    infoMsg='Interuption in the river network, check last reach for break';    
  end
  finishedWithPreviousOrder=1;
  for j=1:length(idxPreviousOrder)                         % For each reach of previous order
    irPO=idxPreviousOrder(j);                                  
    DistToFirstNodes=coord2dist(riverReaches{irPO}(end,:),riverReachesRawFirstNode);
    DistToLastNodes=coord2dist(riverReaches{irPO}(end,:),riverReachesRawLastNode);
    idxFirstNodesUend=find(DistToFirstNodes<=distanceTolerance);  
    idxLastNodesUend=find(DistToLastNodes<=distanceTolerance);
    idxLastNodesUend=setdiff(idxLastNodesUend,idxFirstNodesUend);
    %
    %   If there is one upstream reach only then combine the up and downstream
    %   reach together.  If there is more than one upstream reach then put 
    %   them in the new network
    if length([idxFirstNodesUend idxLastNodesUend])==1  
      finishedWithPreviousOrder=0;
      if length(idxFirstNodesUend)==1
        riverReaches{irPO}=[riverReaches{irPO};riverReachesRaw{idxFirstNodesUend}];
      else
        riverReaches{irPO}=[riverReaches{irPO};riverReachesRaw{idxLastNodesUend}(end:-1:1,:)];
      end
    elseif length([idxFirstNodesUend idxLastNodesUend])>1
      for k=1:length(idxFirstNodesUend)
        ir=ir+1;
        riverReaches{ir}=riverReachesRaw{idxFirstNodesUend(k)};
        riverTopology(ir,1:3)=[ir riverOrder irPO];
      end
      for k=1:length(idxLastNodesUend)
        ir=ir+1;
        riverReaches{ir}=riverReachesRaw{idxLastNodesUend(k)}(end:-1:1,:);
        riverTopology(ir,1:3)=[ir riverOrder irPO];
      end      
    end
    if length([idxFirstNodesUend idxLastNodesUend])>0
      riverReachesRaw([idxFirstNodesUend idxLastNodesUend])=[];
      riverReachesRawFirstNode([idxFirstNodesUend idxLastNodesUend],:)=[];
      riverReachesRawLastNode([idxFirstNodesUend idxLastNodesUend],:)=[];  
    end
  end
end

%calculate the length of each reach

for ir=1:length(riverReaches)
  reachlength=sum(sqrt(diff(riverReaches{ir}(:,1)).^2 + diff(riverReaches{ir}(:,2)).^2));
  riverTopology(ir,4)=[reachlength];
end;

%warning messages

if infoval==5
  disp('Warning in getRiverTopology :  Cannot get the river topology because of the following reason :');
  disp(['  ' infoMsg]);
end



%--------------------------------------------------------------------------
%--------------------------------------------------------------------------
%---------------   Require Function ---------------------------------------
function [D]=coord2dist(c1,c2);

% coord2dist                - distance matrix from coordinates (Jan 1,2001)
%
% Computes the Euclidean distance matrix between two sets of coordinates.
%
% SYNTAX :
%
% [D]=coord2dist(c1,c2);
%
% INPUT :
%
% c1      n1 by d   matrix of coordinates for the locations in the first set.
%                   A line corresponds to the vector of coordinates at a location,
%                   so the number of columns is equal to the dimension of the space.
%                   There is no restriction on the dimension of the space.
% c2      n2 by d   matrix of coordinates for the locations in the second set, using
%                   the same conventions as for c1. 
%
% OUTPUT :
%
% D       n1 by n2  Euclidean distance matrix between the coordinates in c1 and c2.
%                   The number of lines and columns for D corresponds to the number
%                   of lines in c1 and the number of lines in c2, respectively.
%
% NOTE :
%
% It is possible to specify additional n1 by 1 and n2 by 1 index vectors, taking
% integer values from 1 to nv. The values in the index vectors specify which one
% of the nv variable is known at each one of the corresponding coordinates. The c1
% and c2 matrices of coordinates and the index vectors are then grouped together
% using the MATLAB cell array notation, so that c1={c1,index1} and c2={c2,index2}.

isarray=iscell(c1);
if isarray==1,
  c1=c1{1};
  c2=c2{1};
end;

n1=size(c1,1);
n2=size(c2,1);
unit1=ones(n1,1);
unit2=ones(n2,1);
D=kron(unit2,c1)-kron(c2,unit1);
if size(D,2)==1,
  D=abs(D);
else
  D=sqrt(sum((D.^2)')');
end;
D=reshape(D,n1,n2);

%%%%%%%%%% Make '.exe' file for BMEGUI %%%%%%%%%%%%%%%
% Get River distance for Covariance Modeling
