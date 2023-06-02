function [covparam,aicval] = lscovfit(lag,covval,weight,covmdl)

% lscovfit - Calculate covariance parameter using weighted least square fit
%            (Dec 24,2010)
%
% [covparam,aicval] = lscovfit(lag,covval,weight,covmdl,flgNugget)
%
% INPUT:
%
% lag(1 by m): Spatial lag
% covval(1 by m): Experimental covariance
% numpair(1 by m): Number of pairs used to calculate experimental covariance
% weigh(1 by m): Weight for general least square covarinace model fit
% flgNugget(true/false): If true nugget effect
%
% OUTPUT :
%
% covparam(1 by 2 or 3): Covariance parameter
% aicval(scalar): AIC
%
slopeval = ((covval(3) - covval(2)) / (lag(3) - lag(2)));
intcep = covval(2) - slopeval * lag(2);
init_nugget = min([covval(1)-intcep,0]);
init_sill = min([intcep,covval(1)]);
init_range = max(lag)/2;

% initval = [init_nugget,init_sill,init_range];  
 initval = [init_sill,init_range];            % Remove NUGGET
lb = [realmin,realmin,realmin];
ub = [covval(1)*2,covval(1)*2,lag(end)*1000];
numparam = 3;

fmops = optimset('Algorithm','interior-point','DerivativeCheck','off',...
                 'Diagnostic','off','Display','off','FunValCheck','on',...
                 'MaxFunEvals',10000,'MaxIter',4000,...
                 'TolX',1e-16,'TolFun',1e-16);

% fmops = optimset('Algorithm','active-set','DerivativeCheck','off',...
%                  'Diagnostic','off','Display','off','FunValCheck','off',...
%                  'GradObj','on','MaxFunEvals',10000,'MaxIter',4000,...
%                  'TolX',1e-16,'TolFun',1e-16);

% fmops = optimset('Algorithm','trust-region-reflective','DerivativeCheck','off',...
%                  'Diagnostic','off','Display','off','FunValCheck','on',...
%                  'GradObj','on','MaxFunEvals',10000,'MaxIter',4000,...
%                  'Hessian','user-supplied','TolX',1e-16,'TolFun',1e-16);

switch covmdl
    case 'exponential'
        lsfunc = @objfunexponential;
    case 'gaussian'
        lsfunc = @objfungaussian;
    case 'spherical'
        lsfunc = @objfunspherical;
    case 'exppow'
        lsfunc = @objfunexppow;
        initval = [initval,1];
        lb = [lb,realmin];
        ub = [ub,2];
        numparam = 4;
    case 'exppownn'
        lsfunc = @objfunexppownn;
        initval = [covval(1),init_range,1];
        lb = [realmin,realmin,realmin];
        ub = [covval(1)*2,lag(end)*1000,2];
        numparam = 3;
    otherwise
        error('Invalid covmdl');
end

[covparam,fval,exitflag,output] = ...
    fmincon(@(x) lsfunc(x,lag,covval,weight),initval,[],[],[],[],...
            lb,ub,[],fmops);

if exitflag < 1
    disp(output.message);
end

numsample = size(lag,2);
aicval = numsample * log(fval/numsample) + 2 * numparam;
