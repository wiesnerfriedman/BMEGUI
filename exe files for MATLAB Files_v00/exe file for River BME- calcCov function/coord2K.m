function [K]=coord2K(c1,c2,model,param,dist,filtmodel);

% coord2K                   - covariance/variogram matrix from coordinates (Jan 1,2001)
%
% Compute the covariance or variogram matrix between two sets of coordinates,
% based on the Euclidean distances between these sets of coordinates.
%
% SYNTAX :
%
% [K]=coord2K(c1,c2,model,param,dist,filtmodel);
%
% INPUT :
%
% c1        n1 by d  matrix of coordinates for the locations in the first set.
%                    A line corresponds to the vector of coordinates at a location,
%                    so the number of columns is equal to the dimension of the space.
%                    There is no restriction on the dimension of the space.
% c2        n2 by d  matrix of coordinates for the locations in the second set, using
%                    the same conventions as for c1. 
% model     string   that contains the name of the variogram or covariance model that
%                    is used for the computation.
% param     k by 1   vector of values for the parameters of model, according to the
%                    convention for the corresponding variogram or covariance model
%                    (see the MODELS directory).
%dist      cell      optional parameter giving the function to calculate distances.
%                    dist{1} is the name of the distance function, such that the
%                    distances between the points c1 and c2 is calculated using 
%                    [d]=eval([dist{1},'(c1,c2)']), where c1 and c2 are n1 by 2 and 
%                    n2 by 2 matrices, respectively, and d is a n1 by n2 matrix of 
%                    distances between c1 and c2.
%                    dist{2} is an optional parameter. Hence when length(dist)>1, then the
%                    distance matrix is calculated using [d]=eval([dist{1},'(c1,c2,dist{2})'])
%                    Note: The default is dist={'coord2dist'}       
%
%filtmodel scalar    optional value specifying if the stochastic model component is to
%                    be included (filtmodel=1) or is to be filtered out (filtmodel=0).
%                    See the krigingfilter.m function in the KRIGING directory.
%
%               
%
% OUTPUT :
%
% K         n1 by n2 covariance or variogram matrix between the coordinates in c1 and c2,
%                    depending on the fact that model is a covariance or a variogram model.
%                    The number of lines and columns for K corresponds to the number of
%                    lines in c1 and the number of lines in c2, respectively.
%
% NOTE :
%
% For a detailed discussion about the coding of the model and param variables in
% various situations (e.g., nested models, multivariate case, space/time case),
% the reader is referred to the detailed description given for the kriging.m
% function in the KRIGING directory.

%%%%%% Build the general data structure for singular cases
if nargin<5
  dist{1}={'coord2dist'};
end;

noindex=(iscell(c1)==0);
nocell=(iscell(model)==0);

if nocell==1 & noindex==1,          % case for 1 model and 1 variable
  n1=size(c1,1);                    % compute the number of coordinates
  n2=size(c2,1);
  c1={c1,ones(n1,1)};               % create an index with values 1
  c2={c2,ones(n2,1)};
  nm=1;                             % set number of models equal to 1
  nv=1;                             % set number of variables equal to 1
  model={model};                    % create a cell array with model
  np=length(param);                 % create a cell array with param
  param={{param(1),param(2:np)}};
end;

if nocell==1 & noindex==0,          % case for 1 model and nv variables
  n1=size(c1{1},1);                 % compute the number of coordinates
  n2=size(c2{1},1);
  nm=1;                             % set number of models equal to 1
  nv=max([c1{2};c2{2}]);            % compute the number of variables 
  model={model};                    % create a cell array with model
  param={param};
end;

if nocell==0 & noindex==1,          % case for nm models and 1 variable
  n1=size(c1,1);                    % compute the number of coordinates
  n2=size(c2,1);
  c1={c1,ones(n1,1)};               % create an index with values 1
  c2={c2,ones(n2,1)};
  nm=length(model);                 % compute the number of models 
  nv=1;                             % set the number of variables to 1 
  for i=1:nm,                       % create cell arrays with param vectors 
    np=length(param{i});               
    param{i}={param{i}(1),param{i}(2:np)};
  end;
end;

if nocell==0 & noindex==0;          % case for nm models and nv variables
  n1=size(c1{1},1);                 % compute the number of coordinates
  n2=size(c2{1},1);
  nm=length(model);                 % compute the number of models
  nv=max([c1{2};c2{2}]);            % compute the number of variables 
end;

%%%%%% Initialize the parameters

if nargin<6,
  filtmodel=ones(nm,1);
end;

%%%%%% compute the distance matrix and initialize the size of K

[isST,isSTsep,modelS,modelT]=isspacetime(model);

if isSTsep,
  [nparam,models]=nparammodels;
end;

if (n1==0)|(n2==0),
  K=zeros(n1,n2);
  return;
else
  if ~isST,
    switch dist{1}
      case 'coord2dist'
        D=eval([dist{1},'(c1{1},c2{1})']);
      case 'coord2distRiver'
        D=eval([dist{1},'(c1{1}(:,1:4),c2{1}(:,1:4),dist{2})']);
    end;
    K=zeros(size(D));
  else
    nd=size(c1{1},2)-1;
    switch dist{1}
      case 'coord2dist'
        Ds=eval([dist{1},'(c1{1}(:,1:nd),c2{1}(:,1:nd))']);
        Dt=coord2dist(c1{1}(:,nd+1),c2{1}(:,nd+1));
      case 'coord2distRiver'
        Ds=eval([dist{1},'(c1{1}(:,1:4),c2{1}(:,1:4),dist{2})']);
        Dt=coord2dist(c1{1}(:,nd+1),c2{1}(:,nd+1));
    end;
    K=zeros(size(Ds));
  end;
end;  

%%%%%% Compute the covariance matrix or vector

if n1==n2,                            % test if K is symmetric and set
  issymm=prod(prod(double(c1{1}==c2{1})));    % issymm to 0 or 1 
else
  issymm=0;
end;

if issymm==1,                 % if K is symmetric 
  for i=1:nv,                 % loop twice over the number of variables
    indexi=find(c1{2}==i);    % and the number of models by selecting the 
    for j=i:nv,               % appropriate subset of locations and model
      indexj=find(c2{2}==j);  % and use the symmetry property of the matrix
      for k=1:nm,
        if filtmodel(k)==1,
	  C=param{k}{1};
          if ~isST,
	    K(indexi,indexj)=K(indexi,indexj)+eval...
                              ([model{k},'(D(indexi,indexj),[C(i,j),param{k}{2}])']);
          end;
          if (~isSTsep)&(isST),
	    K(indexi,indexj)=K(indexi,indexj)+eval...
            ([model{k},'(Ds(indexi,indexj),Dt(indexi,indexj),[C(i,j),param{k}{2}])']);
          end;
          if isSTsep,
            nps=nparam{strmatch(modelS{k},models,'exact')};
            npt=nparam{strmatch(modelT{k},models,'exact')};
	    Ks=eval([modelS{k},'(Ds(indexi,indexj),[1,param{k}{2}(1:nps-1)])']);
	    Kt=eval([modelT{k},'(Dt(indexi,indexj),[1,param{k}{2}(nps:nps+npt-2)])']);
            K(indexi,indexj)=K(indexi,indexj)+C(i,j)*Ks.*Kt;
          end;
        end;
      end;
      if i~=j,
        K(indexj,indexi)=K(indexi,indexj)';
      end;
    end;
  end;
else                          % else K is not symmetric
  for i=1:nv,                 % loop twice over the number of variables
    indexi=find(c1{2}==i);    % and the number of models by selecting the
    for j=1:nv,               % appropriate subset of locations and model
      indexj=find(c2{2}==j);
      for k=1:nm,
        if filtmodel(k)==1,
	  C=param{k}{1};
          if ~isST,
            K(indexi,indexj)=K(indexi,indexj)+eval...
                              ([model{k},'(D(indexi,indexj),[C(i,j),param{k}{2}])']);
          end;
          if (~isSTsep)&(isST),
	    K(indexi,indexj)=K(indexi,indexj)+eval...
            ([model{k},'(Ds(indexi,indexj),Dt(indexi,indexj),[C(i,j),param{k}{2}])']);
          end;
          if isSTsep,
            nps=nparam{strmatch(modelS{k},models,'exact')};
            npt=nparam{strmatch(modelT{k},models,'exact')};
	    Ks=eval([modelS{k},'(Ds(indexi,indexj),[1,param{k}{2}(1:nps-1)])']);
	    Kt=eval([modelT{k},'(Dt(indexi,indexj),[1,param{k}{2}(nps:nps+npt-2)])']);
            K(indexi,indexj)=K(indexi,indexj)+C(i,j)*Ks.*Kt;
          end;
        end;
      end;
    end;
  end;
end;

