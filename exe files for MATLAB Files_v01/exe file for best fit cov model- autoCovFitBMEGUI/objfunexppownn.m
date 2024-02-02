function fval = objfunexppownn(covparam,lag,covval,weight)

% objfunexponential - Objective function of exponential model for 
%                     least square fit (Mar 30,2011)
%
% fval = objfunexponential(covparam,lag,covval,weight)
%
% INPUT:
%
% covparam(1 by 4): Covariance parameter
% lag(1 by m): Spatial lag
% covval(1 by m): Experimental covariance
% weigh(1 by m): Weight for general least square covarinace model fit
%
% OUTPUT :
%
% fval(scalar): Value of objective function
%

covmdlval = exppowC(lag,covparam);

fval = weight*((covmdlval - covval).^2)';
