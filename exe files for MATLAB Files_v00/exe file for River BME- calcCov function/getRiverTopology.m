function [riverReaches,riverTopology,infoval,infoMsg]=getRiverTopology(riverReachesRaw,sRiverOutlet,...
  distanceTolerance)

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
if nargin<4, name='riverTopology'; end;
if nargin<3, distanceTolerance=0; end;

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
