function RIVERLIBtest(testType)
%RIVERLIBtest                     - function to test the riverlib directory
%
%SYNTAX:
%RIVERLIBtest(testType);
%
%INPUT:
%testType         scalar          1=Sinusoidal Network (single reach)
%                                 2=Complex Network (many reaches)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
if nargin<1
  for testType=1:2    
    RIVERLIBtestSUB(testType);
  end;
else
  RIVERLIBtestSUB(testType);
end;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function RIVERLIBtestSUB(testType)
%RIVERLIBtestSUB                   - function to test the riverlib directory
%
%SYNTAX:
%RIVERLIBtest(testType);
%
%INPUT:
%testType         scalar          1=Sinusoidal Network (single reach)
%                                 2=Complex Network (many reaches)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
switch testType
case 1
  [riverReachesRaw,matrix,idx]=generateSin(1);   
  range=15;  
  pb=100;
  pTolerance=0.5;
  disp('**********TEST A: Single Reach Test**********');
case 2
  [riverReachesRaw]=generateRiverReachesRaw(4,10);   
  range=0.1; 
  pb=4;
  pTolerance=0.05;
  disp('**********TEST B: Complex Network Test**********');
end;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


disp('Test 1: Obtaining River Topology for a Network');
sRiverOutlet=riverReachesRaw{1}(1,:);
distanceTolerance=0;
[riverReaches,riverTopology,infoval,infoMsg]=getRiverTopology(riverReachesRaw,sRiverOutlet,...
    distanceTolerance);
figure; hold on;
plotRiverNetwork(riverReachesRaw,riverTopology,1);
title('Un-organized River Reaches');
figure; hold on;
plotRiverNetwork(riverReaches,riverTopology,1);
title('Re-organized Upstream --> Downstream');
riverTopology
disp('Test 1: Complete; See Figures 1 & 2');

disp('Test 2: Associating points along a river network');
if testType==1
  ch=matrix;
  ch(:,3)=1;
else
  ch=cell2matrix(riverReaches);
  [iu,dup]=finddupli(ch);
  dup=cell2matrix(dup);
  ch(dup,:)=[];
  ch(:,3)=1;
end;
[c1,c2]=cartesian2riverProj(riverReaches,ch);
RiverPoints=c1(1:3,:)
disp('Test 2: Complete');

disp('Test 3: Snapping Points to a Network');
chs=ch(1:pb:length(ch),1:2)+0.1;
chs(:,3)=1;
figure;hold on;
plotRiverNetwork(riverReaches);
plot(chs(:,1),chs(:,2),'ro');
[c1s,c2s]=cartesian2riverProj(riverReaches,chs,pTolerance);
plot(c1s(:,1),c1s(:,2),'g*','MarkerSize',10);
title('Snapping Data Points to a River Reach');
legend('River Network','Original Data Locations','Snapped Locations');
chs
c1s
disp('Test 3: Complete; See Figure 3');

disp('Test 4: Simulating Data Along a River Reach');
randn('state',6);
nSim=1;
sill=1;
range=range;
covmodel='exponentialC';
covparam=[sill range];    
metric1={'coord2dist'};
metric2={'coord2distRiver',riverTopology};
[Zh,L]=simuchol(c1,covmodel,covparam,1,metric2);
cmap=hot(64);cmap=cmap(end:-1:1,:);
newaxis=[min(c1(:,1))-0.5 max(c1(:,1))+0.5 min(c1(:,2))-0.5 max(c1(:,2))+0.5];
coloraxis=[-2 2];
figure;hold on;
plotRiverNetwork(riverReaches,riverTopology,5);
colorplot(c1(:,1:2),Zh,cmap);
caxis(coloraxis);
colorbar;
title('Simulated Data River Metric'); 
disp('Test 4: Complete; See Figure 4');    

disp('Test 5: Estimating Covariance Using a River Metric');
vZh=var(Zh); mZh=mean(Zh); 
a1Zh=sqrt(sill/vZh); 
a0Zh=-a1Zh*mZh; 
Zh=a0Zh + a1Zh*Zh;
cMS=c1(:,1:4);    
tME=1;
rLag=      [0.0:0.2:4.0]';
rLagTol=   [0.0:0.4:8.0]';
[Cr npr]=stcov(Zh,cMS,tME,Zh,cMS,tME,rLag,rLagTol,0,0,metric2);
figure; hold on;
plot(rLag,Cr(:,1),'r.-','MarkerSize',10);       
hold on;
r=[0:0.1:rLag(end)];
modelplot(r,covmodel,covparam);
title('Estimated Spatial Covariance vs. Spatial Covariance Model');
legend('Covariance Using River Metric','Covariance Model');
xlabel('spatial lag {\itr} (degree)');
ylabel('C({\itr},\tau=0)');
disp('Test 5: Complete; See Figure 5');

disp('Test 6: Testing BME Cross Validation Procedure');
cs=[]; softpdftype=2; nl=[]; limi=[]; probdens=[]; order=NaN; nsmax=0; options(8)=2; ckOption=1;
nhmax=2;
dmax=60;    
[momentsXval,info,MSE,MAE,ME]=BMEprobaMomentsXvalidation(ckOption,c1,cs,Zh,softpdftype,...
   nl,limi,probdens,covmodel,covparam,nhmax,nsmax,dmax,order,metric2,options);
xkBME=momentsXval(:,1);
vkBME=momentsXval(:,2);
MSEr=MSE;
figure; hold on;
plotRiverNetwork(riverReaches,riverTopology,5);
colorplot(c1(:,1:2),xkBME,cmap);
caxis(coloraxis);
colorbar;
title('BME Cross Validation Using River Metric & Simulated river Data');
axis(newaxis); 
disp(sprintf('Mean Square Error = %2.2f',MSEr));
disp('Test 6: Complete; See Figure 6');

disp('Test 7: Testing BME Estimation Procedure');
if testType==1
  c1sub=c1(idx,:);
  Zhsub=Zh(idx);
else
  c1sub=c1(1:4:length(c1),:);
  Zhsub=Zh(1:4:length(c1));
end;
[moments,info]=BMEprobaMoments(c1,c1sub,cs,Zhsub,softpdftype,nl,limi,probdens,...
    covmodel,covparam,nhmax,nsmax,dmax,order,metric2,options);
BMEval=moments(:,1);
BMEvar=moments(:,2);
figure; hold on;
plotRiverNetwork(riverReaches);
colorplot(c1(:,1:2),BMEval,cmap);
caxis(coloraxis);
colorbar;
title('BME Estimation Using River Metric & Simulated river Data');
axis(newaxis);
figure; hold on;
plotRiverNetwork(riverReaches);
coloraxis=[0 1];
colorplot(c1(:,1:2),BMEvar,cmap);
caxis(coloraxis);
colorbar;
plot(c1sub(:,1),c1sub(:,2),'b.','MarkerSize',12);
title('BME Estimation Posterior Variance');
axis(newaxis); 
disp('Test 7: Complete; See Figures 7 & 8');

disp('Test 8: Obtaining River Network Statistics');
[rS]=getRiverStats(riverReaches,riverTopology);
rS
disp('Test 8: Complete');

disp('All riverlib Tests Successful.');
disp('Please Review the figures created as part of the test procedure');



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [riverReachesRaw,matrix]=generateRiverReachesRaw(norder,np)
% generateRiverReachesRaw         - randomly generate a river network
%
% generateRiverReaches will randomly generate a river network for use in generating river topology
% and parameters used in coord2distRiver
%
% SYNTAX:
%
% [riverReachesRaw]=generateRiverReachesRaw(order,np)
%
% INPUT:
%
% norder           1 x 1             highest order of generated network
%                                    default is 6
% np               1 x 1             number of points to connect between nodes
%                                    default is 10
%
% OUTPUT:
%
% riverReachesRaw  nr cell array     each cell{i} is an np(i) x 2 matrix containing x,y coordinates
%                                    of points connecting the river network.  
%                                    number of cells = nr (number of total reaches)
% matrix           np x 2            matrix of x,y coordinates dissolved into a single matrix

if nargin<1  norder=6; end
if nargin<2  np=10; end


rand('state',0);

%             j=1       j=2       j=3       j=4
%  i=4     (4,1)     (4,2)     (4,3)     (4,4)
%  i=3          (3,1)     (3,2)     (3,3)
%  i-2               (2,1)     (2,2)    
%  i=1                    (1,1)                

x0=-75;
y0=40;
dx=1.5;
dy=1;

ir=1;                     % reach number
for i=1:norder-1          % go from reach order 1 to higest order
  iNEXT=i+1;
  nj=iNEXT;                 % remaining reaches available of order i
  for j=1:i               %    go from j=1 to i
    if j<i
      n=randperm(nj+1);     %
      n=n(1)-1;             %        select how many upstream connection to (i,j) there is
    else
      n=nj;
    end
    for jNEXT=iNEXT-nj+1:iNEXT-nj+n
      ir=ir+1;
      x=[(-i/2-1+j) (-iNEXT/2-1+jNEXT)]*dx+x0;
      y=[i iNEXT]*dy+y0;
      x=[x(1):diff(x)/(np-1):x(2)]';
      if np>6
        xS=x(3:end-2);
        x(3:end-2)= x(3:end-2) ...
          + dx/10 * sin( [0:pi/(length(xS)-1):pi]' ) .* ( 0.5*rand(size(xS)) + cos( xS/dx*2*pi*3) );
      end
      y=[y(1):diff(y)/(np-1):y(2)]';
      riverReachesRaw{ir}=[x,y];
    end;
    nj=nj-n;
  end;
end;

% Generate Outlet Reach coordinates and add 'river' curvature to straight lines

nr=length(riverReachesRaw);
x=[(-1/2-1+1/2) (-1/2-1+1)]*dx+x0;
x=[x(1):diff(x)/(np-1):x(2)]';
if np>6
  xS=x(3:end-2);
  x(3:end-2)= x(3:end-2) ...
    + dx/10 * sin( [0:pi/(length(xS)-1):pi]' ) .* ( 0.5*rand(size(xS)) + cos( xS/dx*2*pi*3) );
end
y=[0 1]*dy+y0;
y=[y(1):diff(y)/(np-1):y(2)]';
%riverReachesRaw{nr+1}=[x,y];
%riverReachesRaw([2:nr+1])=riverReachesRaw;
riverReachesRaw{1}=[x y];


% Reverse the upstream/downstream points of some reaches at random

nr=length(riverReachesRaw);
nrSwaped=randperm(nr);
for ir=1:floor(nr/2)
  riverReachesRaw{nrSwaped(ir)}=riverReachesRaw{nrSwaped(ir)}(end:-1:1,:);
end;
[matrix]=cell2matrix(riverReachesRaw);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [riverReachesRaw,matrix,idx]=generateSpiral
%
%generates spiral network for testing riverlib functions
%
%
a0=1; a1=1; nt=500; dt=0.1; r0=2; r1=.1;
t=0:dt:nt*dt;
a=a0+a1*t;
r=r0+r1*t;
x=r.*cos(t);
y=r.*sin(t);
matrix=[x',y'];
pnum=8;
newidx=[];
k=1;
for i=1:pnum:length(matrix)
  newidx(k)=i;
  k=k+1;
end;       
idx=sort(newidx);
riverReachesSpiral{1}=matrix;
riverReachesRaw=riverReachesSpiral;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [riverReachesRaw,matrix,idx]=generateSin(subtype)
%
%generates a sinusoidal reach for testing riverlib functions
%
%
%INPUT:
%subtype            scalar        1=cross validation points
%                                 2=validation points
%
%OUTPUT:
%riverReachesRaw    cell array    xy coordinates making up the river reach to use as input in getRiverTopology.m
%
%matrix             np x 2        xy matrix of points making up river reach
%
%idx                idx x 2       matrix of points to be used in cross-validation or validation
%
dx=0.01; x0=0; xL=10; Yo=6.5; Yp=4; dlr=0.5;
% simulate the river topography
x=x0:dx:xL;
y=Yo*sin(x/Yp*2*pi);
% put points equidistant
l=[0 cumsum(sqrt(diff(x).^2+diff(y).^2))];
lr=0:dlr:l(end);
xr=interp1(l,x,lr);
yr=interp1(l,y,lr);
%select subset of points for xvalidation or validation (1 or 2)
switch subtype
   case 1
     % select the data for the cross validation procedure
     nr=length(lr);
     nDC=10;
     idx=1:nDC:nr;                
   case 2
     % select the training data for the validation procedure
     nr=length(lr);
     nDT=4;
     idx=1:nDT:nr;                
end;
matrix=[xr',yr'];
riverReachesSin{1}=matrix;
riverReachesRaw=riverReachesSin;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
