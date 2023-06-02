function [rS] = getRiverStats(riverReaches,riverTopology,basinfile)
% getRiverStats                   - calculates a series of statistics related to a river network
%
%SYNTAX:  [rS] = getRiverStats(riverReaches,riverTopology,basinfile);
%
%INPUT:
%riverReaches       cell        x,y points that make up the river network
%
%riverTopology      nr x 4      Topology for each reach.  nr=# of reaches
%                               Column 1: Unique Reach ID
%                               Column 2: Order #
%                               Column 3: Downstream Reach ID
%                               Column 4: Total Reach Length
%
%basinfile          char        name of basin boundary file (i.e. basin.e00)
%                               export from GIS as Interchange File(e00)
%
%OUTPUT:
%rS                 1 x 5       matrix of calculated network statistics
%                               Column 1: Total # of reaches
%                               Column 2: Total river length within study area
%                               Column 3: Average Meandering Ratio (= straight length/river length)
%                               Column 4: Branching Level (measure of network complexity, more branches=more complex)
%                               Column 5: River Ratio (measure of network complexity, 
%                                         basin area/network length, 0 = simple, 1 = most complex)
%
%%%%%%%%%%%%%%%%%%%%%%5

if nargin<3,disp('Continuing without boundary file, River Ratio will not be calculated');end;
if nargin<2,error('please generate river Topology using getRiverTopology.m');end;

%calculate total # of reaches
sizeRT=size(riverTopology);
numreaches=sizeRT(1);

%calculate total network length
netlength=sum(riverTopology(:,4));

%calculate meandering ratio
MEratio=[];
for ir=1:length(riverReaches)
  straightDIST=coord2dist(riverReaches{ir}(1,:),riverReaches{ir}(end,:));  
  riverDIST=riverTopology(ir,4);
  MEratio=[MEratio;riverDIST/straightDIST];
end;
avMEratio=mean(MEratio);

%calculate branching level
brlevel=max(riverTopology(:,2));

%calculate river ratio
if nargin<3
  riverRatio=NaN;
else
  pVal=readArce00(basinfile);
  [xypoints]=cell2matrix(pVal);
  basinarea=polyarea(xypoints(:,1),xypoints(:,2));
  riverRatio=basinarea/netlength;
end;


rS=[numreaches netlength avMEratio brlevel riverRatio];

  