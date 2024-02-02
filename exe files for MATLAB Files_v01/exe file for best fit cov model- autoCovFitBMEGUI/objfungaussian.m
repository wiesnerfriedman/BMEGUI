function fval = objfungaussian(covparam,lag,covval,weight)

% objfungaussian - Objective function of gaussian model for 
%                  least square fit (Mar 30,2011)
%
% fval = objfungaussian(covparam,lag,covval,weight)
%
% INPUT:
%
% covparam(1 by 3): Covariance parameter
% lag(1 by m): Spatial lag
% covval(1 by m): Experimental covariance
% weigh(1 by m): Weight for general least square covarinace model fit
%
% OUTPUT :
%
% fval(scalar): Value of objective function
%

% covmdlval = nuggetC(lag,covparam(1)) + ...
%             gaussianC(lag,[covparam(2),covparam(3)]);
covmdlval =  gaussianC(lag,[covparam(1),covparam(2)]); % Removed NUGGET

fval = weight*((covmdlval - covval).^2)';
