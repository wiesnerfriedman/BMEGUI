function [estvalues]=estZsBME(meantrend,covariance,harddata,softdata,estgrid,esttime,estparam);

% estZsBME                  - BME estimation of a S/TRF X(s,t) over space at a given time t=tk
%
% calculale the space/time BME estimate showing the spatial distribution of the Space/Time Random
% Field (S/TRF) Z(s,t) at a given time t=tk.  The map shows the distribution of the estimated 
% values Z(s,t).  The spatial location of the estimation grid may be provide as input, or they
% may be automatically generated by the program. This program works in the purely spatial
% case by specifying an estimation time of tk=NaN.
%
% SYNTAX :
%
% [estvalues]=estZsBME(meantrend,covariance,harddata,softdata,estgrid,esttime,estparam);
%
% INPUT :
%
% meantrend={cMSm,tMEm,ms,mt,griddatamethod} or [] or filename
%
%                           meantrend specifies the spatiotemporal mean trend if it is
%                           known.  meantrend is either a cell array of input values
%                           as shown above, a or an empty set or a character string with the name of a file.
%                           When meantrend is empty the mean trend does not exist and is not used.
%                           Otherwise the input values are provided by the cell array or from the file,
%                           allowing to calculate the mean trend and remove it from the data.
%                           If an input value is not provided by the cell array or the file then it's default
%                           value is used. The input values of estvalues are the following:
%   cMSm        nMSm x 2    spatial coordinates of nMSm monitoring stations for which the mean trend is know
%   tMEm        nMEm x 1    time of nMEm event for which the mean trend is known
%   ms          nMSm x 1    value of the spatial component of the mean trend at the nMSm monitoring stations
%                           See help stmean for explanation on how to calculate ms, and help stmeaninterp for 
%                           more explanations on how it is used to calculate the mean trend
%   mt          1 x nMEm    value of the temporal component of the mean trend for the nMEm monitoring event
%   griddatamethod          char string defining the option used in the griddata.m function.
%                           See help griddata for the possible values of this option.
%                           The default is griddatamethod='linear';
%
%
% covariance={covmodel,covparam} or filename
%
%                           covariance specifies the spatiotemporal covariance.  meantrend is either a cell 
%                           array or a character string with the name of a MATLAB binary data file (*.mat).
%                           covariance provide the of input values shown above. These input values 
%                           are the following:
%
%   covmodel  char or cell  char string or cell of char strings defining the covariance model.
%                           See modelsyntax for more explanations on covariance models.
%   covparam  vec or cell   vector of input parameter if covmodel is a char string, or cell of vectors if 
%                           covmodel is a cell of char strings.
%
%
% harddata={ph,zh}; or filename.mat or filename.dat
%
%                           harddata contains the hard data of the S/TRF X(s,t).  It is either a cell array   
%                           with the input values shown above, or a character string with the name of a file.
%                           When harddata is a character string it should correspond to the name of a file with
%                           an extension of either *.mat or *.dat. The *.mat file must correspond to a MATLAB
%                           binary data file containing the input variables as described below. The *.dat
%                           file must correspong to a file in GeoEAS format which will be read with the 
%                           readGeoEAS function, with the first column for the spatial coordinates, then a column
%                           for 'time' if time exists, then with one column of hard data values.
%                           The input values of harddata are the following:
%   ph           nh x ndim  space/time coordinates of the hard data points
%   zh           nh x 1     values of S/TRF X(s,t) at the hard data points.
%
%
% softdata={ps,softpdftype,nl,limi,probdens}; or filename.mat or filename.dat
%
%                           softdata contains the soft data of the S/TRF X(s,t).  It is either a cell array   
%                           with the input values shown above, or a character string with the name of a file.
%                           When softdata is a character string it should correspond to the name of a file with
%                           an extension of either *.mat or *.dat. The *.mat file must correspond to a MATLAB
%                           binary data file containing the input variables as described below. The *.dat
%                           file must correspong to a file in Proba format which will be read with the 
%                           readProba function. The input values of harddata are the following:
%   ps           ns x ndim  space/time coordinates of the soft data points
%   softpdftype  scalar     indicates the type of soft pdf representing the  
%                           probabilitic soft data at the coordinates specified in ps.  
%                           softpdftype may take value 1, 2, 3 or 4, as follow:
%                           1 for Histogram, 2 for Linear, 3 for Grid histogram, 
%                           and 4 for Grid Linear. (see probasyntax for more explanations)
%   nl           ns by 1    vector of the number of interval limits. nl(i) is the number  
%                           of interval limits used to define the soft pdf for soft data 
%                           point i. (see probasyntax for more explanations)
%   limi         ns by l    matrix of interval limits, where l is equal to
%                           either max(nl) or 3 depending of the softpdftype.
%                           limi(i,:) are the limits of intervals for the i-th 
%                           soft data. (see probasyntax for more explanations)
%   probdens     ns by p    matrix of probability density values, where p is 
%                           equal to either max(nl)-1 or max(nl), depending on the 
%                           softpdftype. probdens(i,:) are the values of the probability 
%                           density corresponding to the intervals for the i-th soft data 
%                           defined in limi(i,:). (see probasyntax for more explanations)
%
%
% estgrid=sk;  or [] or {nxpix,nypix,incldatapts,inclvoronoi,ax}
%
%                           estgrid specifies the spatial estimation grid. estgrid is either a 
%                           numerical matrix of nk spatial location of estimation points, or empty,
%                           or a cell array with the input values shown above. When estgrid is a 
%                           nk x nspacedim numerical matrix it defines sk, the spatial coordinates
%                           of the estimation points, and only these locations will be used.
%                           Otherwise estgrid is a cell defining parameters that are used to automatically
%                           generate the spatial estimation grid, with input values described below.
%                           If estgrid is empty then the grid is automatically generated with the default
%                           value of input values described below.
%
%   nxpix          scalar   integer specifying the number of pixel in the x direction used to create the
%                           estimation grid. The default value is 20
%   nypix          scalar   integer specifying the number of pixel in the y direction used to create the
%                           colored maps. The default value is 20
%   incldatapts    scalar   optional integer indicating whether to include data points in the estimation 
%                           points. 1 to include them, 0 o.w.. The default is 0.
%   inclvoronoi    scalar   optional integer indicating whether to include in the estimation the points  the
%                           points corresponding the voronoi diagram of the data points. 1 to include them, 
%                           0 o.w.. The default is 0.
%   ax        4x1 or empty  ax=[xmin xmax ymin ymax] specifies the spatial area to view.  If ax is empty
%                           then the viewing area is set to comprise all the estimation and data points.
%                           The default value is []
%
%
% esttime=tk
%                           esttime specifies the estimation time tk for Space/time fields.  In the
%                           case of a purely spatial random variable X(s), use esttime=NaN
%
%
% estparam={nhmax,nsmax,dmax,order,options}; or []. default is []
%
%                           estparam specifies the estimation parameters. estparam is either a 
%                           cell array with the input values shown above, or empty, or it is not specified.
%                           When estparam is a empty or is not specified then default input values are used.
%                           Otherwise estparam is a cell defining estimation parameters that are used to 
%                           in the BME estimate calculation, with input values described below. See
%                           help BMEprobaMoments for more explanation on these parameters
%
%   nhmax      scalar       maximum number of hard data values that are considered
%                           for the estimation points. Default is 20
%   nsmax      scalar       maximum number of soft data values that are considered for
%                           the estimation at the locations specified in ck. Default is 3
%   dmax       1 by 1 or 3  In the purely spatial case, dmax is the maximum distance between 
%                           an estimation location and existing hard/soft data locations.
%                           For space/time data, the dmax variable becomes a 1 by 3 vector instead of a single
%                           scalar value. dmax(1) is the maximum spatial distance between an estimation location
%                           and existing data locations, and dmax(2) is the maximum temporal lag between an
%                           estimation location and existing data locations. Only data locations that respect
%                           both conditions are considered for the estimation. dmax(3) refers to a space/time
%                           metric, such that the space/time distance=spatial distance+dmax(3)*temporal
%                           distance. The definition of this space/time distance is needed for selecting the
%                           nhmax closest data locations from the estimation locations.
%   order      scalar       order of the polynomial drift along the spatial axes at the
%                           estimation locations. For the zero-mean case, NaN (Not-a-Number)
%                           is used. Default is NaN
%   options    1 by 1 or 14 vector of optional parameters that can be used.  See 
%                           help BMEprobaMoments for more explanation on these parameters
%                           The default are those provide by the BMEoptions function (see
%                           help BMEoptions for more explanations)
%
% OUTPUT :
%
% estvalues={pk,XkBME,XkBMEv,varianceX};
%
%                           estvalues contains the BME mean estimate of the S/TRF X(s,t).  It is a    
%                           cell array with the following cell elements
%
%   sk          nk x 2      spatial coordinates of the estimation point
%   XkBME       nk x 1      BME estimated value of the S/TRF X(s,t) at the estimation points
%   XkBMEv      nk x 1      estimation error variance of the S/TRF X(s,t) at the estimation points
%   varianceX   scalar      variance of the data X.

%
% Set preferences
%
if nargin<1, meantrend=[]; end;
if nargin<2, covariance=[]; end;
if nargin<3, harddata=[]; end;
if nargin<4, softdata=[]; end;
if nargin<5, estgrid=[]; end;
if nargin<6, esttime=NaN; end;
if nargin<7, estparam=[]; end;

if isempty(covariance)
  error('covariance was not defined');
end

if isempty(harddata) & isempty(softdata) 
  error('both harddata and softdata are empty');
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Check input parameters for errors and extract values from each input parameters
%

%
%  Get the mean trend model from meantrend={cMSm,tMEm,ms,mt,griddatamethod} or filename
%
if isempty(meantrend)
  % no mean trend model
elseif ischar(meantrend)
  load(meantrend);
  if ~exist('cMSm','var') | ~exist('tMEm','var') | ~exist('ms','var') | ~exist('mt','var') 
    error(sprintf('The file %s must contain the variables cMSm, tMEm, ms and mt',meantrend));
  end
  if ~exist('griddatamethod','var'), griddatamethod='linear'; end
elseif iscell(meantrend),
  if ~isnumeric(meantrend{1}), error('meantrend{1} must be numeric'); end;
  cMSm=meantrend{1};
  [nMSm n2]=size(cMSm);
  if nMSm<3, error('meantrend{1} must have at least three rows'); end;
  if n2~=2, error('meantrend{1} must have two columns'); end;
  if length(meantrend)<4
    error('meantrend must be a cell array of at least four cells'); 
  end
  if ~isnumeric(meantrend{1}) | ~isnumeric(meantrend{2}) | ~isnumeric(meantrend{3}) | ~isnumeric(meantrend{4})
    error('the first for cells elements of meantrend must be numeric'); 
  end
  tMEm=meantrend{2};
  [n1 nMEm]=size(tMEm);
  if n1~=1, error('meantrend{2} must have one row'); end;
  ms=meantrend{3};
  [n1 n2]=size(ms);
  if n1~=nMSm, error('meantrend{3} must have same number of rows as meantrend{1}'); end;
  if n2~=1, error('meantrend{3} must have one column'); end;
  mt=meantrend{4};
  [n1 n2]=size(mt);
  if n1~=1, error('meantrend{4} must have one column'); end;
  if n2~=nMEm, error('meantrend{4} must have same size as meantrend{2}'); end;
  if length(meantrend)<5
    griddatamethod='linear'; 
  else
    griddatamethod=meantrend{5};
    if ~ischar(meantrend{5}), error('meantrend{5} must be a char string'); end;
  end
else
  error('meantrend must be either a char string or a cell array'); 
end

%
%  Get the covariance model from covariance={covmodel,covparam} or filename
%
if ischar(covariance)
  load(covariance);
  if ~exist('covmodel','var') | ~exist('covparam','var')
    error(sprintf('The file %s must contain the variables covmodel and covparam',covariance));
  end
elseif iscell(covariance),
  if length(covariance)<2
    error('covariance must be a cell array of at least two cells'); 
  end
  if ~( ischar(covariance{1}) & isnumeric(covariance{2}) ) & ~( iscell(covariance{1}) & iscell(covariance{2}) )
    error('covariance{1} and covariance{2} must be both either a char string and a numeric vector, or two cells'); 
  end;
  covmodel=covariance{1};
  covparam=covariance{2};
else
  error('covariance must be either a char string or a cell array'); 
end

[isST,isSTsep,modelS,modelT]=isspacetime(covmodel);

%
%  Get the hard data from harddata={ph,zh} or filename.mat or filename.dat
%
if isempty(harddata)
  ph=[];
  zh=[];
elseif ischar(harddata)
  if length(harddata)>=4 & isequal(harddata(end-3:end),'.mat')
    load(harddata);
    if ~exist('ph','var') | ~exist('zh','var')
      error(sprintf('The file %s must contain the variables ph and zh',harddata));
    end
  elseif length(harddata)>=4 & isequal(harddata(end-3:end),'.dat')
    [val,valname,filetitle]=readGeoEAS(harddata);
    ph=val(:,1:end-1);
    zh=val(:,end);
  else
    error(sprintf('The file name %s must end with .mat or .dat',harddata));
  end
elseif iscell(harddata),
  if length(harddata)<2
    error('harddata must be a cell array of at least two cells'); 
  end
  if ~isnumeric(harddata{1}), error('harddata{1} must be numeric'); end;
  ph=harddata{1};
  [nh n2]=size(ph);
  if ~isnumeric(harddata{2}), error('harddata{2} must be numeric'); end;
  zh=harddata{2};
  [n1 n2]=size(zh);
  if n1~=nh, error('harddata{2} must have same number of rows as harddata{1}'); end;
else
  error('harddata must be either a char string or a cell array'); 
end
[nh dimh]=size(ph);

%
%  Get the soft data from softdata={ps,softpdftype,nl,limi,probdens} or filename.mat or filename.dat
%
if isempty(softdata)
  ps=[];
  softpdftype=1;
  nl=[];
  limi=[];
  probdens=[];
elseif ischar(softdata)
  if length(softdata)>=4 & isequal(softdata(end-3:end),'.mat')
    load(softdata);
    if ~exist('ps','var') | ~exist('softpdftype','var') | ~exist('nl','var') | ...
        ~exist('limi','var') | ~exist('probdens','var')
      error(sprintf('The file %s must contain the variables ps, softpdftype, nl, limi and probdens',softdata));
    end
  elseif length(softdata)>=4 & isequal(softdata(end-3:end),'.dat')
    [ps,isSTsoftdata,softpdftype,nl,limi,probdens,filetitle]=readProba(softdata);
    ps=ps{1};
    if isST~=isSTsoftdata
      error('covariance model and soft data disagree, one is space/time while not the other');
    end
  else
    error(sprintf('The file name %s must end with .mat or .dat',softdata));
  end
elseif iscell(softdata),
  if length(softdata)~=5
    error('softdata must be a cell array of five cells'); 
  end
  ps=softdata{1};
  softpdftype=softdata{2};
  nl=softdata{3};
  limi=softdata{4};
  probdens=softdata{5};
else
  error('softdata must be either a char string or a cell array'); 
end
[ns dims]=size(ps);
 
if nh>0 & ns>0 & dimh~=dims
  error('hard and soft data points do not seem to have the same space/time dimension');
elseif nh~=0
  dimdata=dimh;
else
  dimdata=dims;
end

%
%  Get the estimation grid from 
% estgrid=pk or {nxpix,nypix,incldatapts,inclvoronoi,ax}
%
if isempty(estgrid),
  sk=[];
  nxpix=20;
  nypix=20;
  incldatapts=1;
  inclvoronoi=1;
  ax=[];
elseif isnumeric(estgrid),
  sk=estgrid;
  [nk dimkspace]=size(sk);
  if dimkspace~=dimdata-isST, 
    error('estimation grid and data points do not seem to have the same spatial dimension');
  end
  ax=[];
elseif iscell(estgrid),
  sk=[];
  if length(estgrid)>=1
    nxpix=estgrid{1};
  else
    nxpix=20;
  end    
  if length(estgrid)>=2
    nypix=estgrid{2};
  else
    nypix=20;
  end    
  if length(estgrid)>=3
    incldatapts=estgrid{3};
  else
    incldatapts=1;
  end    
  if length(estgrid)>=4
    inclvoronoi=estgrid{4};
  else
    inclvoronoi=1;
  end    
  if length(estgrid)>=5
    ax=estgrid{5};
  else
    ax=[];
  end    
else
  error('estgrid must be either numeric or a cell array'); 
end

%
%  Get the estimation time from esttime
%
tk=esttime;
if length(tk)~=1, error('tk should be a scalar'); end

%
%  Get the estimation parameters from 
%     estparam={nhmax,nsmax,dmax,order,options};
%
if isempty(estparam),
  nhmax=20;
  nsmax=3;
  dmax=[];
  order=NaN;
  options=BMEoptions;
elseif iscell(estparam),
  if length(estparam)>=1
    nhmax=estparam{1};
  else
    nhmax=20;
  end    
  if length(estparam)>=2
    nsmax=estparam{2};
  else
    nsmax=3;
  end    
  if length(estparam)>=3
    dmax=estparam{3};
  else
    dmax=[];
  end    
  if length(estparam)>=4
    order=estparam{4};
  else
    order=NaN;
  end    
  if length(estparam)>=5
    options=estparam{5};
  else
    options=BMEoptions;
  end    
else
  error('estparam must be a cell array'); 
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Assign default values
%

%
% Assign default value of ax defining the viewing window
%
if isempty(ax) & dimdata==2+isST
  pdata=[ph;ps];
  sdata=unique(pdata(:,1:2),'rows');
  if isempty(sk)
    ax=[min(sdata(:,1)) max(sdata(:,1)) min(sdata(:,2)) max(sdata(:,2))];
  elseif dimdata==2+isST
    ax=[min(sk(:,1)) max(sk(:,1)) min(sk(:,2)) max(sk(:,2))];
  end
end

%
% Assign default estimation grid
%
if isempty(sk)
  if dimdata~=2+isST
    error('Can only determine an estimation grid for 2D spatial case, use estgrid=sk');
  end
  pdata=[ph;ps];
  sdata=unique(pdata(:,1:2),'rows');
  [xg yg]=meshgrid(ax(1):diff(ax(1:2))/nxpix:ax(2),ax(3):diff(ax(3:4))/nypix:ax(4));
  sk=[xg(:) yg(:)];
  if incldatapts
    sk=unique([sk;sdata],'rows');
  end
  if inclvoronoi
    [vx,vy] = voronoi(sdata(:,1),sdata(:,2));
    sv=unique([vx(:) vy(:)],'rows');
    idx= (ax(1)<=sv(:,1)) & (sv(:,1)<=ax(2)) & (ax(3)<=sv(:,2)) & (sv(:,2)<=ax(4)) ;
    sv=sv(idx,:);
    sk=[sk;sv];
  end
  nk=size(sk,1);
  if nk==0, error('the construction of the estimation grid resulted in no estimation points'); end
end
if isST
  if isnan(tk), error('the covariance model is spatiotemporal yet the estimation time was not specified'); end
  pk=[sk tk*ones(nk,1)];
else
  pk=sk;
end

%
% set the default values of the estimation paramaters estparam={nhmax,nsmax,dmax,order,options};
%
if isnan(nhmax) nhmax=20; end
if isnan(nsmax) nsmax=3; end
if isempty(dmax) | isnan(dmax)
  if ~isST & length(ax)==4
    dmax(1)=max([diff(ax(1:2)) diff(ax(3:4))]);
  elseif ~isST
    if ~iscell(covmodel) & length(covparam)<2, 
      error('Cannot determine dmax(1)');
    elseif ~iscell(covmodel)
      dmax(1)=10*covparam(2)
    else
      dmax(1)=0;
      for i=1:length(covmodel)
        if length(covparam{1})>1, dmax(1)=max([dmax(1) covparam{1}(2)]); end
      end
      if dmax(1)==0, error('Cannot determine dmax(1)'); end;
    end
  else  % s/t case
    if ~iscell(covmodel) & length(covparam)<3
      error('Cannot determine dmax');
    elseif ~iscell(covmodel)
      dmax=[100*covparam(2) 100*covparam(3) covparam(2)/covparam(3)];
    elseif iscell(covmodel) & length(covparam{1})<3
      error('Cannot determine dmax');
    else
      dmax=[100*covparam{1}(2) 100*covparam{1}(3) covparam{1}(2)/covparam{1}(3)];
    end
  end
end
if isempty(order) order=NaN; end
if isempty(options), options=BMEoptions; end;


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% remove the S/T mean from the data
%

%[moments,info]=BMEprobaMoments(pk,ph,ps,zh,softpdftype,nl,limi,probdens,covmodel,covparam,nhmax,nsmax,dmax,order,options);
if ~isempty(meantrend)
  dimspace=dimdata-isST;
  if dimspace~=2
    error('When using mean trend the data must be 2D in space')
  end
  if isST & isnan(tk)
    error('When doing space time estimation you must provide the estimation time tk in estgrid{6}');
  end
  if ~isST
    tk=0;
    mt=0;
    tMEm=0;
    phI=[ph tk*ones(nh,1)];
    psI=[ps tk*ones(ns,1)];
  else
    phI=ph;
    psI=ps;
  end
  msthard=stmeaninterpstv(cMSm,tMEm,ms,mt,phI);
  zh=zh-msthard;
  mstsoft=stmeaninterpstv(cMSm,tMEm,ms,mt,psI);
  limi=probaoffset(softpdftype,nl,limi,-mstsoft);
end

[moments,info]=BMEprobaMoments(pk,ph,ps,zh,softpdftype,nl,limi,probdens,...
  covmodel,covparam,nhmax,nsmax,dmax,order,options);

XkBME=moments(:,1);
XkBME(isnan(XkBME))=0;

XkBMEv=moments(:,2);

%
% Re-ajust XkBME by adding the mean to the estimated values
%
if ~isempty(meantrend)
  if ~isST
    pkI=[pk tk*ones(nk,1)];
  else
    pkI=pk;
  end
  mstest=stmeaninterpstv(cMSm,tMEm,ms,mt,pkI);
  XkBME=XkBME+mstest;
else
  mstest=NaN*XkBME;
end

%
% calculating varianceX
%
if ~iscell(covmodel)
  varianceX=covparam(1);
else
  varianceX=0;
  for i=1:length(covmodel)
    varianceX=varianceX+covparam{1}(1);
  end
end  

%
%  Re-ajust XkBMEv by setting NaN to max varianceX
%
XkBMEv(isnan(XkBMEv))=varianceX;
if ~isreal(XkBMEv), 
  XkBMEv=real(XkBMEv); 
end;

estvalues={sk,XkBME,XkBMEv,tk,varianceX};

if dimdata==2+isST & options(1)==1,
  if isempty(harddata) | isempty(ph)
    harddataplot=[];
  else
    sh=ph(:,1:2);
    if isST
      th=ph(:,3);
      idxh= (th==tk);
      sh=ph(idxh,1:2);
      zh=zh(idxh);
    else
      sh=ph;
      ss=ps;
    end
    harddataplot={sh,zh};
  end
  if isempty(softdata) | isempty(ps)
    softdataplot=[];
  else
    ss=ps(:,1:2);
    [zs,softvar]=proba2stat(softpdftype,nl,limi,probdens);
    if isST
      ts=ps(:,3);
      idxs= (ts==tk);
      ss=ps(idxs,1:2);
      zs=zs(idxs);
    else
      ss=ps;
    end
    softdataplot={ss,zs};
  end
  plotZsBME(estvalues,1,2,[],[],[],harddataplot,softdataplot);
end






















